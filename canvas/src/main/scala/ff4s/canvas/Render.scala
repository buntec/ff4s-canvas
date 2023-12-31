package ff4s.canvas
package render

import cats.effect.*
import cats.effect.implicits.*
import cats.effect.std.Dispatcher
import cats.kernel.Eq
import cats.syntax.all.*
import fs2.Stream
import fs2.concurrent.Signal
import fs2.concurrent.SignallingRef
import fs2.dom.Dom
import fs2.dom.HtmlCanvasElement
import fs2.dom.ResizeObserver
import org.scalajs.dom

case class Settings(
    minZoom: Double = 0.2,
    maxZoom: Double = 5.0,
    zoomSensitivity: Double = 0.001,
    relMargin: Double = 0.01
)

def loop[F[_]: Dom, D: Eq](
    canvas: HtmlCanvasElement[F],
    dispatcher: Dispatcher[F],
    data: Signal[F, D],
    drawFrame: (DOMHighResTimeStamp, D) => Draw[Unit],
    config: Settings
)(using F: Async[F]): Resource[F, Unit] = for {

  sizeRef <- (canvas.offsetWidth, canvas.offsetHeight)
    .flatMapN((w, h) => SignallingRef.of[F, (Double, Double)]((w, h)))
    .toResource

  _ <- ResizeObserver[F]((a, _) =>
    sizeRef.set((a.head.contentRect.width, a.head.contentRect.height))
  ).evalTap(_.observe(canvas.asInstanceOf[fs2.dom.Element[F]]))

  currentAndPrevData <- (data.get, F.realTime)
    .flatMapN((d, t) => F.ref((d, t, d, t)))
    .toResource

  _ <- data.changes.discrete
    .evalMap(d =>
      F.realTime.flatMap(t =>
        currentAndPrevData.update((d1, t1, _, _) => (d, t, d1, t1))
      )
    )
    .compile
    .drain
    .background

  _ <- sizeRef.discrete
    .switchMap { (width, height) =>
      Stream
        .bracket(F.delay:
          var keepGoing = true

          val compiler = Compiler(
            canvas.asInstanceOf[dom.HTMLCanvasElement],
            width.toInt,
            height.toInt,
            config
          )

          def draw(t: Double): Unit =
            val setup = (
              dsl.save,
              dsl.clearRect(0, 0, width, height),
              dsl.marginTransform.flatMap(_.applyToCtx)
            ).tupled

            val cleanup = dsl.restore

            dispatcher.unsafeRunAndForget(
              currentAndPrevData.get.flatMap: (data, _, _, _) =>
                F.delay:
                  (setup *> drawFrame(DOMHighResTimeStamp(t), data) *> cleanup)
                    .foldMap(compiler)
                  if (keepGoing) {
                    dom.window.requestAnimationFrame(draw _): Unit
                  }
            )

          draw(0)

          F.delay { keepGoing = false }
        )(identity)
        .evalMap(_ => F.never)
    }
    .compile
    .drain
    .background

} yield ()
