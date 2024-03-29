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
package example1

import cats.effect.Async
import cats.effect.implicits.*
import cats.effect.std.Dispatcher
import cats.kernel.Eq
import cats.syntax.all.*
import ff4s.canvas.*
import fs2.Stream
import fs2.dom.Dom
import monocle.syntax.all.*
import org.http4s.Uri

object scatter:
  val config = ScatterPlot.Config(
    nXTicks = 10,
    nYTicks = 10,
    tickFont = Font(FontSize.Px(12), FontFamily.Name("Roboto"))
      .withWeight(FontWeight.Light),
    gridColor = Color.Gray,
    textColor = Color.White,
    axisColor = Color.Silver,
    legend = true
  )

  val genTrace: Gen[ScatterPlot.Trace] =
    for
      label <- Gen.alphaNumericString(10)
      nPoints <- Gen.between(5, 50)
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
    yield ScatterPlot.Trace(points, marker, label.some)

  case class State(
      traces: Option[List[ScatterPlot.Trace]] = None,
      genS: Gen.S = Gen.setSeed(17L).run(Gen.init)(0)
  )

case class State[F[_]](
    uri: Option[Uri] = None,
    canvas: Option[fs2.dom.HtmlCanvasElement[F]] = None,
    scatterPlot: scatter.State = scatter.State()
)

sealed trait Action[F[_]]

object Action:

  case class Noop[F[_]]() extends Action[F]

  case class SetCanvas[F[_]](canvas: fs2.dom.HtmlCanvasElement[F])
      extends Action[F]

  case class SetScatterPlotData[F[_]](
      traces: List[ScatterPlot.Trace]
  ) extends Action[F]

  case class RandomizeData[F[_]]() extends Action[F]

trait View[F[_]] extends Buttons[State[F], Action[F]]:
  dsl: ff4s.Dsl[State[F], Action[F]] =>

  import html.*

  val view = useState: state =>
    div(
      cls := "min-h-screen flex flex-col items-center bg-gray-800 text-gray-100 font-sans font-thin",
      h1(cls := "m-4 text-3xl", "ff4s-canvas"),
      div(
        cls := "m-2 flex flex-col items-center gap-2",
        h2(cls := "text-2xl", "Scatter Plot"),
        canvasTag(
          cls := "border rounded border-gray-500 sm:w-[500px] sm:h-[400px] md:w-[600px] md:h-[500px] lg:w-[800px] lg:h-[600px] w-[300px] h-[300px]",
          idAttr := "scatter-plot",
          key := "scatter-plot",
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
          Gen
            .between(1, 4)
            .flatMap(n => scatter.genTrace.replicateA(n))
            .run(state.scatterPlot.genS)
        state.focus(_.scatterPlot.genS).replace(nextState) ->
          store.dispatch(Action.SetScatterPlotData(traces))
      case (Action.SetScatterPlotData(traces), state) =>
        state.focus(_.scatterPlot.traces).replace(traces.some) -> F.unit
      case (Action.SetCanvas(canvas), state) =>
        state.copy(canvas = canvas.some) -> F.unit
    )

    _ <- store.state
      .map(_.canvas)
      .discrete
      .unNone
      .changes(Eq.fromUniversalEquals)
      .switchMap(canvas =>
        Stream
          .resource(
            ff4s.canvas.ScatterPlot(
              scatter.config,
              store.state.map(_.scatterPlot.traces.getOrElse(Nil)),
              canvas,
              dispatcher
            )
          )
          .evalMap(_ => F.never)
      )
      .compile
      .drain
      .background

    _ <- store.dispatch(Action.RandomizeData()).toResource
  yield store
