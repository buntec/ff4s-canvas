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

import cats.effect.Async
import cats.effect.implicits.*
import cats.effect.std.Dispatcher
import cats.kernel.Eq
import cats.syntax.all.*
import ff4s.canvas.*
import fs2.Stream
import math.abs
import fs2.dom.Dom
import monocle.syntax.all.*
import org.http4s.Uri
import org.scalajs.dom
import fs2.concurrent.SignallingRef
import examples.VolSkewChart.DrawResult

object volskew:

  val config = VolSkewChart.Config(
    nXTicks = 10,
    nYTicks = 10,
    tickFont = Font(FontSize.Px(12), FontFamily.Name("Roboto"))
      .withWeight(FontWeight.Light),
    gridColor = Color.Gray,
    textColor = Color.White,
    axisColor = Color.Silver
  )

  val genTrace: Gen[VolSkewChart.Trace] =
    for
      nPoints <- Gen.between(5, 50)
      x0 <- Gen.normal
      xs <- Gen.normal
        .replicateA(nPoints)
        .map(_.map(math.abs(_)))
        .map(dxs => dxs.scanLeft(x0)(_ - _))
      ys <- Gen.pure(xs.map(math.pow(_, 2)))
      points <- Gen.pure((xs zip ys).map(Point.apply))
      fill <- Gen.boolean
      color <- Color.genHue(60.0, 60.0)
      markerSize = 12
      marker <- Gen.choose(
        Marker.Circle(markerSize, color, fill),
        Marker.Triangle(markerSize, color, fill),
        Marker.Square(markerSize, color, fill),
        Marker.Cross(markerSize, color)
      )
    yield VolSkewChart.Trace(points, marker)

  case class State(
      trace: Option[VolSkewChart.Trace] = None,
      genS: Gen.S = Gen.setSeed(17L).run(Gen.init)(0)
  )

case class State[F[_]](
    uri: Option[Uri] = None,
    canvas: Option[fs2.dom.HtmlCanvasElement[F]] = None,
    volskewPlot: volskew.State = volskew.State()
)

sealed trait Action[F[_]]

object Action:

  case class Noop[F[_]]() extends Action[F]

  case class SetCanvas[F[_]](canvas: fs2.dom.HtmlCanvasElement[F])
      extends Action[F]

  case class SetVolSkewChatData[F[_]](traces: VolSkewChart.Trace)
      extends Action[F]

  case class RandomizeData[F[_]]() extends Action[F]

  case class UpdatePoint[F[_]](p: Point, newY: Double) extends Action[F]

trait View[F[_]] extends Buttons[State[F], Action[F]]:
  dsl: ff4s.Dsl[State[F], Action[F]] =>

  import html.*

  val view = useState: state =>
    div(
      cls := "min-h-screen flex flex-col items-center bg-gray-800 text-gray-100 font-sans font-thin",
      h1(cls := "m-4 text-3xl", "ff4s-canvas"),
      div(
        cls := "m-2 flex flex-col items-center gap-2",
        h2(cls := "text-2xl", "Vol Skew Plot"),
        canvasTag(
          cls := "border rounded border-gray-500 sm:w-[500px] sm:h-[400px] md:w-[600px] md:h-[500px] lg:w-[800px] lg:h-[600px] w-[300px] h-[300px]",
          idAttr := "volskew-plot",
          key := "volskew-plot",
          insertHook := (el =>
            Action.SetCanvas(el.asInstanceOf[fs2.dom.HtmlCanvasElement[F]])
          )
        ),
        "Use your mouse or touchpad to pan and zoom.",
        btn("randomize", Action.RandomizeData())
      )
    )

class App[F[_]: Dom](using F: Async[F])
    extends ff4s.App[F, State[F], Action[F]]
    with View[F]:

  override val store = for
    dispatcher <- Dispatcher.sequential[F]

    store <- ff4s.Store[F, State[F], Action[F]](State())(store =>
      case (Action.Noop(), state) => state -> F.unit
      case (Action.RandomizeData(), state) =>
        val (nextState, traces) =
          volskew.genTrace
            .run(state.volskewPlot.genS)
        state.focus(_.volskewPlot.genS).replace(nextState) ->
          store.dispatch(Action.SetVolSkewChatData(traces))
      case (Action.SetVolSkewChatData(trace), state) =>
        state.focus(_.volskewPlot.trace).replace(trace.some) -> F.unit
      case (Action.SetCanvas(canvas), state) =>
        state.copy(canvas = canvas.some) -> F.unit
      case (Action.UpdatePoint(point, newY), state) =>
        state
          .focus(_.volskewPlot.trace)
          .replace(
            state.volskewPlot.trace.map: trace =>
              trace.copy(points =
                trace.points.filterNot(_.x == point.x) :+ Point(point.x, newY)
              )
          ) -> F.unit
    )

    drawRes <- SignallingRef.of[F, Option[DrawResult]](None).toResource

    draggedPoint <- SignallingRef.of[F, Option[Point]](None).toResource

    _ <- store.state
      .map(_.canvas)
      .discrete
      .unNone
      .flatMap(_ => drawRes.discrete.unNone)
      .evalTap:
        case DrawResult(mt, mouseCalc, xScale, yScale, hovered) =>
          F.delay:
            dom.document
              .getElementById("volskew-plot")
              .addEventListener[dom.MouseEvent](
                "mouseup",
                md =>
                  dispatcher.unsafeRunAndForget:
                    draggedPoint.set(None)
              )
      .evalTap:
        case DrawResult(mt, mouseCalc, xScale, yScale, hovered) =>
          F.delay:
            dom.document
              .getElementById("volskew-plot")
              .addEventListener[dom.MouseEvent](
                "mousemove",
                md =>
                  dispatcher.unsafeRunAndForget:
                    val mp0 = mouseCalc(md)
                    val mp = mt.invert(mp0)
                    val x = xScale.inverse(mp.x)
                    val y = yScale.inverse(mp.y)
                    val mouse = Point(x, y)
                    draggedPoint.get.flatMap:
                      _.foldMapM: dp =>
                        store.state.get
                          .map(_.volskewPlot.trace)
                          .flatMap:
                            _.foldMapM: trace =>
                              trace.points
                                .find(_.x == dp.x)
                                .foldMapM: found =>
                                  store.dispatch(
                                    Action.UpdatePoint(found, mouse.y)
                                  )
              )
      .evalTap:
        case DrawResult(mt, mouseCalc, xScale, yScale, hovered) =>
          F.delay:
            dom.document
              .getElementById("volskew-plot")
              .addEventListener[dom.MouseEvent](
                "mousedown",
                md =>
                  dispatcher.unsafeRunAndForget:
                    val mp0 = mouseCalc(md)
                    val mp = mt.invert(mp0)
                    val x = xScale.inverse(mp.x)
                    val y = yScale.inverse(mp.y)
                    val mouse = Point(x, y)
                    store.state.get
                      .map(_.volskewPlot.trace)
                      .flatMap: trace =>
                        trace.foldMapM: points =>
                          points.points
                            .minByOption(_.distanceTo(mouse))
                            .foldMapM: p =>
                              hovered.foldMapM: hp =>
                                F.whenA(p == hp)(draggedPoint.set(Some(hp)))
              )
      .compile
      .drain
      .background

    _ <- store.state
      .map(_.canvas)
      .discrete
      .unNone
      .changes(Eq.fromUniversalEquals)
      .switchMap(canvas =>
        Stream
          .resource(
            VolSkewChart(
              volskew.config,
              store.state.map(_.volskewPlot.trace.get),
              canvas,
              dispatcher
            )
          )
          .evalMap:
            _.discrete.unNone
              .evalMap: drawR =>
                drawRes.set(Some(drawR))
              .compile
              .drain
      )
      .compile
      .drain
      .background

    _ <- store.dispatch(Action.RandomizeData()).toResource
  yield store
