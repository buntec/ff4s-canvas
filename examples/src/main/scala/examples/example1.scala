package example1

import cats.effect.Async
import cats.effect.implicits._
import cats.effect.std.Dispatcher
import cats.syntax.all._
import ff4s.canvas
import fs2.Stream
import org.http4s.Uri
import org.scalajs.dom

case class State(
    uri: Option[Uri] = None,
    canvas: Option[dom.HTMLCanvasElement] = None,
    data: Option[List[ff4s.canvas.Point]] = None
)

sealed trait Action

object Action {

  case object Noop extends Action

  case class SetCanvas(canvas: dom.HTMLCanvasElement) extends Action

  case class SetData(data: List[ff4s.canvas.Point]) extends Action

  case class RandomizeData() extends Action

}

class App[F[_]](implicit val F: Async[F]) extends ff4s.App[F, State, Action] {

  override val store = for {

    dispatcher <- Dispatcher.sequential[F]

    nextDouble = F.delay(scala.util.Random.nextDouble())
    nextPoint = (nextDouble, nextDouble).mapN((x, y) => ff4s.canvas.Point(x, y))

    store <- ff4s.Store[F, State, Action](State())(store =>
      _ match {
        case Action.Noop => _ -> None
        case Action.RandomizeData() =>
          _ -> nextPoint
            .replicateA(10)
            .flatMap(points => store.dispatch(Action.SetData(points)))
            .some
        case Action.SetData(points)   => _.copy(data = points.some) -> none
        case Action.SetCanvas(canvas) => _.copy(canvas = canvas.some) -> none
      }
    )

    getTraces = store.state.get.map { s =>
      val points = s.data.getOrElse(Nil)
      val trace = ff4s.canvas.ScatterPlot
        .Trace(points, ff4s.canvas.Color.Keyword("green"))
      List(trace)
    }

    _ <- store.state
      .map(_.canvas)
      .discrete
      .unNone
      .switchMap(canvas =>
        Stream
          .resource(
            ff4s.canvas.ScatterPlot(getTraces)(canvas)
          )
          .evalMap(_ => F.never)
      )
      .compile
      .drain
      .background

    _ <- store.state.discrete
      .evalMap { state =>
        F.delay(println(s"state: $state"))
      }
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
            Action.SetCanvas(el.asInstanceOf[dom.HTMLCanvasElement])
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
