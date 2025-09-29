/*
 * Copyright 2023 buntec
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package examples
package example2

import cats.effect.implicits.*
import cats.effect.kernel.Async
import cats.effect.kernel.Resource
import cats.effect.std.Dispatcher
import cats.kernel.Eq
import cats.syntax.all.*
import ff4s.canvas.*
import ff4s.canvas.syntax.*
import fs2.Stream
import fs2.concurrent.*
import fs2.dom.Dom
import fs2.dom.HtmlCanvasElement
import monocle.syntax.all.*
import org.http4s.Uri
import org.scalajs.dom
import org.scalajs.dom.MouseEvent

object Chart:

  final case class Config(
      nXTicks: Int,
      nYTicks: Int,
      tickFont: Font,
      axisColor: Color,
      textColor: Color,
      gridColor: Color
  )

  final case class Trace(points: List[Point], marker: Marker)

  object Trace:
    given Eq[Trace] = Eq.fromUniversalEquals
    given Transition[Trace] = Transition.transition((tr1, tr2, t) =>
      Trace(
        Transition[List[Point]](tr1.points, tr2.points, t),
        Transition[Marker](tr1.marker, tr2.marker, t)
      )
    )

  final case class DrawResult(mouse: Point, hovered: Option[Point])

  def apply[F[_]: Dom](
      config: Config,
      trace: Signal[F, Trace],
      elm: HtmlCanvasElement[F],
      dispatcher: Dispatcher[F]
  )(using F: Async[F]): Resource[F, Signal[F, Option[DrawResult]]] =

    def tooltip(hover: Point): Draw[Unit] =
      import Draw.*
      for
        _ <- save
        w <- width
        h <- height
        mp0 <- mousePos
        mt <- marginTransform
        mp = mt.invert(mp0)
        _ <- setFillStyle(config.textColor)
        _ <- setFont(config.tickFont)
        _ <- fillText(
          f"x=${hover.x}%.2f, y=${hover.y}%.2f",
          mp.x + 0.02 * w,
          mp.y - 0.02 * h
        )
        _ <- restore
      yield ()

    val drawFrame = (t: DOMHighResTimeStamp, trace: Trace) =>
      import Draw.*
      val nPoints = trace.points.length
      if nPoints > 0 then
        val xMin = trace.points.map(_.x).min
        val xMax = trace.points.map(_.x).max
        val yMin = trace.points.map(_.y).min
        val yMax = trace.points.map(_.y).max
        for
          _ <- save
          w <- width
          h <- height
          xScale0 = Scale
            .linear(
              Scale.Domain(xMin, xMax),
              Scale.Range(0, w)
            )
            .getOrElse(throw new Exception("failed to build x-scale"))
          yScale0 = Scale
            .linear(
              Scale.Domain(yMin, yMax),
              Scale.Range(h, 0)
            )
            .getOrElse(throw new Exception("failed to build y-scale"))
          trans <- transform
          xScale = trans
            .rescaleX(xScale0)
            .getOrElse(throw new Exception("failed to rescale x-scale"))
          yScale = trans
            .rescaleY(yScale0)
            .getOrElse(throw new Exception("failed to rescale y-scale"))
          xTickSize = 0.01 * h
          yTickSize = 0.01 * w
          _ <- Axes(
            w,
            h,
            xTickSize,
            yTickSize,
            xScale,
            yScale,
            config.nXTicks,
            config.nYTicks,
            config.tickFont,
            config.axisColor,
            config.textColor,
            config.gridColor
          ).draw(Point(0, 0))

          // clip everything outside axes region
          _ <- beginPath
          _ <- rect(yTickSize, 0, w - yTickSize, h - xTickSize)
          _ <- clip

          mp <- mousePos
          _ <- kvDelete("hover")
          _ <-
            trace.points.traverse_ : point =>
              val at = Point(xScale(point.x), yScale(point.y))
              val boundingPath = trace.marker.boundingPath(at)
              isPointInPath(
                boundingPath,
                mp.x,
                mp.y,
                FillRule.Nonzero
              ).flatMap: isHover =>
                if isHover then
                  val c0 = trace.marker.toColor
                  val marker = trace.marker.withColor(c0.lighten(20))
                  for
                    _ <- kvPut("hover", point)
                    _ <- save
                    _ <- setShadowColor(c0)
                    _ <- setShadowBlur(trace.marker.toSize / 2)
                    _ <- marker.draw(at)
                    _ <- restore
                  yield ()
                else trace.marker.draw(at)

          _ <- restore
          hovered <- kvGet[Point]("hover").flatTap(_.foldMapM(tooltip))
          mouse <- (mousePos, marginTransform).tupled.map: (mp, mt) =>
            val mt0 = mt.invert(mp)
            Point(xScale.inverse(mt0.x), yScale.inverse(mt0.y))
        yield Some(DrawResult(mouse, hovered))
      else Draw.pure(Option.empty[DrawResult])

    render.loop(
      elm,
      dispatcher,
      trace,
      drawFrame,
      render.Settings(
        margins = Margins.uniformRelative(0.05),
        disableDrag = true
      )
    )

  val config = Chart.Config(
    nXTicks = 10,
    nYTicks = 10,
    tickFont = Font(FontSize.Px(12), FontFamily.Name("Roboto"))
      .withWeight(FontWeight.Light),
    gridColor = Color.Gray,
    textColor = Color.White,
    axisColor = Color.Silver
  )

  val genTrace: Gen[Chart.Trace] =
    for
      nPoints <- Gen.between(5, 50)
      x0 <- Gen.normal
      points <- (Gen.normal, Gen.normal).tupled
        .replicateA(nPoints)
        .map(_.map((x, y) => Point(x, y)))
      fill <- Gen.boolean
      color <- Color.genHue(60.0, 60.0)
      markerSize = 12
      marker <- Gen.choose(
        Marker.Circle(markerSize, color, fill),
        Marker.Triangle(markerSize, color, fill),
        Marker.Square(markerSize, color, fill),
        Marker.Cross(markerSize, color)
      )
    yield Chart.Trace(points, marker)

  case class State(
      trace: Option[Chart.Trace] = None,
      genS: Gen.S = Gen.setSeed(17L).run(Gen.init)(0)
  )

case class State(
    uri: Option[Uri] = None,
    canvas: Option[dom.HTMLCanvasElement] = None,
    chart: Chart.State = Chart.State()
)

sealed trait Action

object Action:

  case class Noop() extends Action

  case class SetCanvas(canvas: dom.HTMLCanvasElement) extends Action

  case class SetData(traces: Chart.Trace) extends Action

  case class RandomizeData() extends Action

  case class UpdatePoint(old: Point, upd: Point) extends Action

trait View[F[_]] extends Buttons[State, Action]:
  dsl: ff4s.Dsl[State, Action] =>

  import html.*

  val view = useState: state =>
    div(
      cls := "min-h-screen flex flex-col items-center bg-gray-800 text-gray-100 font-sans font-thin",
      h1(cls := "m-4 text-3xl", "ff4s-canvas"),
      div(
        cls := "m-2 flex flex-col items-center gap-2",
        h2(cls := "text-2xl", "Chart"),
        canvasTag(
          cls := "border rounded border-gray-500 sm:w-[500px] sm:h-[400px] md:w-[600px] md:h-[500px] lg:w-[800px] lg:h-[600px] w-[300px] h-[300px]",
          idAttr := "chart",
          key := "chart",
          insertHook := (el =>
            Action.SetCanvas(el.asInstanceOf[dom.HTMLCanvasElement])
          )
        ),
        "Use your mouse or touchpad to zoom and drag points 🚀.",
        btn("randomize", Action.RandomizeData())
      )
    )

class App[F[_]: Dom](using F: Async[F])
    extends ff4s.App[F, State, Action]
    with View[F]:

  override val store = for
    dispatcher <- Dispatcher.sequential[F]

    store <- ff4s.Store[F, State, Action](State())(store =>
      case (Action.Noop(), state) => state -> F.unit
      case (Action.RandomizeData(), state) =>
        val (nextState, traces) =
          Chart.genTrace.run(state.chart.genS)
        state.focus(_.chart.genS).replace(nextState) ->
          store.dispatch(Action.SetData(traces))
      case (Action.SetData(trace), state) =>
        state.focus(_.chart.trace).replace(trace.some) -> F.unit
      case (Action.SetCanvas(canvas), state) =>
        state.copy(canvas = canvas.some) -> F.unit
      case (Action.UpdatePoint(old, upd), state) =>
        state
          .focus(_.chart.trace)
          .replace(
            state.chart.trace.map: trace =>
              trace.copy(points = trace.points.map: p =>
                if p == old then upd else p)
          ) -> F.unit
    )

    drawRes <- SignallingRef.of[F, Option[Chart.DrawResult]](None).toResource

    draggedPoint <- SignallingRef.of[F, Option[Point]](None).toResource

    _ <- draggedPoint.discrete
      .changes(Eq.fromUniversalEquals)
      .unNone
      .evalMap: p =>
        F.delay:
          println(s"dragged x:${p.x}, y: ${p.y}")
      .compile
      .drain
      .background

    _ <- store.state
      .map(_.canvas)
      .discrete
      .changes(Eq.fromUniversalEquals)
      .unNone
      .evalMap: p =>
        F.delay:
          println("canvas update")
      .compile
      .drain
      .background

    _ <-
      store.state
        .map(_.canvas)
        .discrete
        .changes(Eq.fromUniversalEquals)
        .unNone
        .evalMap: canvas =>
          F.delay:
            canvas
              .addEventListener[dom.MouseEvent](
                "mouseup",
                md =>
                  dispatcher.unsafeRunAndForget:
                    draggedPoint.set(None)
              )
        .compile
        .drain
        .background

    _ <-
      store.state
        .map(_.canvas)
        .discrete
        .changes(Eq.fromUniversalEquals)
        .unNone
        .evalMap: canvas =>
          F.delay:
            canvas
              .addEventListener[dom.MouseEvent](
                "mousedown",
                md =>
                  dispatcher.unsafeRunAndForget:
                    drawRes.get.flatMap:
                      _.foldMapM: draw =>
                        draggedPoint.set(draw.hovered)
              )
        .compile
        .drain
        .background

    _ <-
      store.state
        .map(_.canvas)
        .discrete
        .changes(Eq.fromUniversalEquals)
        .unNone
        .evalMap: canvas =>
          F.delay:
            canvas
              .addEventListener[dom.MouseEvent](
                "mousemove",
                md =>
                  dispatcher.unsafeRunAndForget:
                    drawRes.get.flatMap:
                      _.foldMapM: draw =>
                        draggedPoint.get.flatMap:
                          _.foldMapM: old =>
                            draggedPoint.set(Some(draw.mouse)) *>
                              store.dispatch:
                                Action.UpdatePoint(old, draw.mouse)
              )
        .compile
        .drain
        .background

    _ <- store.state
      .map(_.canvas)
      .discrete
      .changes(Eq.fromUniversalEquals)
      .unNone
      .switchMap(canvas =>
        Stream
          .resource(
            Chart(
              Chart.config,
              store.state.map(_.chart.trace.get),
              canvas.asInstanceOf[fs2.dom.HtmlCanvasElement[F]],
              dispatcher
            )
          )
          .evalMap:
            _.discrete
              .evalMap(drawRes.set)
              .compile
              .drain
      )
      .compile
      .drain
      .background

    _ <- store.dispatch(Action.RandomizeData()).toResource
  yield store
