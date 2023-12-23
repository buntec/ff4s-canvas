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
      (scala.math.random(), scala.math.random())
    }

    drawFrame = (t: canvas.DOMHighResTimeStamp, xy: (Double, Double)) => {
      import canvas.dsl.*
      val m = t.toMillis / 1000
      for {
        _ <- save
        w <- width
        h <- height
        xScale0 = canvas.Scale
          .linear(
            canvas.Scale.Domain(0, 10),
            canvas.Scale.Range(0, w)
          )
          .get
        yScale0 = canvas.Scale
          .linear(
            canvas.Scale.Domain(0, 10),
            canvas.Scale.Range(h, 0)
          )
          .get
        trans <- transform
        xScale = trans.rescaleX(xScale0).get
        yScale = trans.rescaleY(yScale0).get
        _ <- canvas
          .Axes(
            w,
            h,
            0.01 * w,
            xScale,
            yScale,
            20,
            20,
            "normal 100 12px system-ui",
            canvas.Color.Keyword("gray"),
            canvas.Color.Keyword("black"),
            canvas.Color.Keyword("gray")
          )
          .draw(canvas.Point(30, 30))
        _ <- (canvas.Shape
          .Circle(
            50,
            canvas.Color.Keyword("black").some,
            canvas.Color.Keyword("blue").some
          ): canvas.Shape)
          .draw(
            ff4s.canvas.Point(
              xScale(5 + 5 * math.sin(m * 2 * math.Pi)),
              yScale(5 + 5 * math.cos(m * 2 * math.Pi))
            )
          )
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
              ff4s.canvas.render.Settings(
                relMargin = 0.05
              )
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
