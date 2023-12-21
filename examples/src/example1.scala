package example1

import cats.effect.Async
import cats.effect.implicits._
import cats.syntax.all._
import org.http4s.Uri

import org.scalajs.dom
import fs2.Stream

import ff4s.canvas
import ff4s.canvas.Drawable.*
import cats.effect.std.Dispatcher

case class State(
    uri: Option[Uri] = None,
    canvas: Option[dom.HTMLCanvasElement] = None
)

sealed trait Action

object Action {

  case object Noop extends Action

  case class SetCanvas(canvas: dom.HTMLCanvasElement) extends Action

}

class App[F[_]](implicit val F: Async[F]) extends ff4s.App[F, State, Action] {

  private val window = fs2.dom.Window[F]

  override val store = for {

    dispatcher <- Dispatcher.sequential[F]

    store <- ff4s.Store[F, State, Action](State())(_ =>
      _ match {
        case Action.Noop => _ -> None
        case Action.SetCanvas(canvas) =>
          _.copy(canvas = canvas.some) -> None
      }
    )

    getData = F.delay {
      println("hi")
      (scala.math.random(), scala.math.random())
    }

    drawFrame = (t: ff4s.canvas.DOMHighResTimeStamp, xy: (Double, Double)) => {
      import ff4s.canvas.dsl.*
      for {
        _ <- save
        _ <- (ff4s.canvas.Shape
          .Circle(
            50,
            ff4s.canvas.Color.Keyword("black").some,
            ff4s.canvas.Color.Keyword("blue").some
          ): ff4s.canvas.Shape)
          .draw(ff4s.canvas.Point(xy(0) * 800, xy(1) * 500))
        _ <- restore
      } yield ()
    }

    _ <- store.state
      .map(_.canvas)
      .discrete
      .unNone
      .switchMap(canvas =>
        Stream
          .resource(
            ff4s.canvas.render.loop(
              canvas,
              dispatcher,
              getData,
              drawFrame,
              ff4s.canvas.render.Settings()
            )
          )
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
            Action.SetCanvas(el.asInstanceOf[dom.HTMLCanvasElement])
          )
        )
      )
    )
  }

}
