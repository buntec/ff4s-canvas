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
import cats.effect.implicits._
import cats.effect.std.Dispatcher
import cats.kernel.Eq
import cats.syntax.all._
import ff4s.canvas
import ff4s.canvas.*
import fs2.Stream
import fs2.dom.Dom
import org.http4s.Uri

object scatter:
  val config = canvas.ScatterPlot.Config(
    nXTicks = 10,
    nYTicks = 10,
    tickFont =
      Font(FontSize.Px(12), FontFamily.SystemUI).withWeight(FontWeight.Thin),
    gridColor = Color.Gray,
    textColor = Color.White,
    axisColor = Color.Silver
  )

case class State[F[_]](
    uri: Option[Uri] = None,
    canvas: Option[fs2.dom.HtmlCanvasElement[F]] = None,
    scatterTraces: Option[List[ScatterPlot.Trace]] = None
)

sealed trait Action[F[_]]

object Action {

  case class Noop[F[_]]() extends Action[F]

  case class SetCanvas[F[_]](canvas: fs2.dom.HtmlCanvasElement[F])
      extends Action[F]

  case class SetScatterPlotData[F[_]](
      traces: List[ScatterPlot.Trace]
  ) extends Action[F]

  case class RandomizeData[F[_]]() extends Action[F]

}

class App[F[_]: Dom](implicit val F: Async[F])
    extends ff4s.App[F, State[F], Action[F]] {

  override val store = for {

    dispatcher <- Dispatcher.sequential[F]

    nextDouble = F.delay(scala.util.Random.nextDouble())
    nextPoint = (nextDouble, nextDouble).mapN((x, y) => ff4s.canvas.Point(x, y))
    nextTrace = nextPoint
      .replicateA(20)
      .map(points =>
        ff4s.canvas.ScatterPlot
          .Trace(points, Marker.Circle(10, Color.Aquamarine, false))
      )

    store <- ff4s.Store[F, State[F], Action[F]](State())(store =>
      _ match {
        case Action.Noop() => _ -> None
        case Action.RandomizeData() =>
          _ -> nextTrace
            .replicateA(3)
            .flatMap(traces =>
              store.dispatch(Action.SetScatterPlotData(traces))
            )
            .some
        case Action.SetScatterPlotData(traces) =>
          _.copy(scatterTraces = traces.some) -> none
        case Action.SetCanvas(canvas) => _.copy(canvas = canvas.some) -> none
      }
    )

    traces = store.state.map { s =>
      s.scatterTraces.getOrElse(Nil)
    }

    _ <- store.state
      .map(_.canvas)
      .discrete
      .unNone
      .changes(Eq.fromUniversalEquals)
      .switchMap(canvas =>
        Stream
          .resource(
            ff4s.canvas.ScatterPlot(scatter.config, traces, canvas, dispatcher)
          )
          .evalMap(_ => F.never)
      )
      .compile
      .drain
      .background

    _ <- store.dispatch(Action.RandomizeData()).toResource

  } yield store

  import dsl._
  import dsl.html._

  object components extends Buttons[F, State[F], Action[F]]
  import components.*

  val heading = h1(cls := "m-4 text-3xl", "Examples")

  override val view = useState { state =>
    div(
      cls := "flex flex-col items-center h-screen bg-gray-800 text-gray-100 font-thin",
      heading,
      div(
        cls := "m-2 flex flex-col items-center gap-2",
        h2(cls := "text-2xl", "Scatter Plot"),
        canvasTag(
          widthAttr := 800,
          heightAttr := 500,
          cls := "border rounded border-gray-500",
          idAttr := "canvas",
          key := "my-canvas",
          insertHook := (el =>
            Action.SetCanvas(el.asInstanceOf[fs2.dom.HtmlCanvasElement[F]])
          )
        ),
        btn("randomize", Action.RandomizeData())
      )
    )
  }

}
