package example1

import cats.effect.Async
import cats.effect.implicits._
import cats.effect.std.Dispatcher
import cats.syntax.all._
import ff4s.canvas
import fs2.Stream
import fs2.dom.Dom
import org.http4s.Uri
import cats.kernel.Eq

case class State[F[_]](
    uri: Option[Uri] = None,
    canvas: Option[fs2.dom.HtmlCanvasElement[F]] = None,
    data: Option[List[ff4s.canvas.Point]] = None
)

sealed trait Action[F[_]]

object Action {

  case class Noop[F[_]]() extends Action[F]

  case class SetCanvas[F[_]](canvas: fs2.dom.HtmlCanvasElement[F])
      extends Action[F]

  case class SetData[F[_]](data: List[ff4s.canvas.Point]) extends Action[F]

  case class RandomizeData[F[_]]() extends Action[F]

}

class App[F[_]: Dom](implicit val F: Async[F])
    extends ff4s.App[F, State[F], Action[F]] {

  override val store = for {

    dispatcher <- Dispatcher.sequential[F]

    nextDouble = F.delay(scala.util.Random.nextDouble())
    nextPoint = (nextDouble, nextDouble).mapN((x, y) => ff4s.canvas.Point(x, y))

    store <- ff4s.Store[F, State[F], Action[F]](State())(store =>
      _ match {
        case Action.Noop() => _ -> None
        case Action.RandomizeData() =>
          _ -> nextPoint
            .replicateA(10)
            .flatMap(points => store.dispatch(Action.SetData(points)))
            .some
        case Action.SetData(points)   => _.copy(data = points.some) -> none
        case Action.SetCanvas(canvas) => _.copy(canvas = canvas.some) -> none
      }
    )

    traces = store.state.map { s =>
      val points = s.data.getOrElse(Nil)
      val trace = ff4s.canvas.ScatterPlot
        .Trace(points, ff4s.canvas.Color.Keyword("green"))
      List(trace)
    }

    _ <- store.state
      .map(_.canvas)
      .discrete
      .unNone
      .changes(Eq.fromUniversalEquals)
      .switchMap(canvas =>
        Stream
          .resource(ff4s.canvas.ScatterPlot(traces, canvas, dispatcher))
          .evalMap(_ => F.never)
      )
      .compile
      .drain
      .background

  } yield store

  import dsl._
  import dsl.html._

  val heading = h1(cls := "m-4 text-4xl", "Canvas")

  override val view = useState { state =>
    div(
      cls := "flex flex-col items-center h-screen",
      heading,
      div(
        cls := "m-2 flex flex-col items-center",
        canvasTag(
          widthAttr := 800,
          heightAttr := 500,
          cls := "border",
          idAttr := "canvas",
          key := "my-canvas",
          insertHook := (el =>
            Action.SetCanvas(el.asInstanceOf[fs2.dom.HtmlCanvasElement[F]])
          )
        ),
        button(
          cls := "border rounded",
          tpe := "button",
          "randomize data",
          onClick := (_ => Action.RandomizeData().some)
        )
      )
    )
  }

}
