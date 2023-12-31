package ff4s.canvas
package render

import cats.effect.*
import cats.effect.implicits.*
import cats.effect.std.Dispatcher
import cats.syntax.all.*
import fs2.Stream
import fs2.concurrent.SignallingRef
import fs2.dom.ResizeObserver
import org.scalajs.dom

case class Settings(
    minZoom: Double = 0.2,
    maxZoom: Double = 5.0,
    zoomSensitivity: Double = 0.001,
    relMargin: Double = 0.01
)

def loop[F[_], D](
    canvas: dom.HTMLCanvasElement,
    dispatcher: Dispatcher[F],
    getData: F[D],
    drawFrame: (DOMHighResTimeStamp, D) => Draw[Unit], // (time, data)
    config: Settings
)(using F: Async[F]): Resource[F, Unit] =
  Stream
    .eval(
      F.delay((canvas.offsetWidth, canvas.offsetHeight))
        .flatMap(SignallingRef.of[F, (Double, Double)](_))
    )
    .flatTap: sizeRef =>
      Stream
        .resource(
          ResizeObserver[F]((a, _) =>
            sizeRef.set((a.head.contentRect.width, a.head.contentRect.height))
          ).evalTap(_.observe(canvas.asInstanceOf[fs2.dom.Element[F]]))
        )
    .flatMap(_.discrete)
    .switchMap: (width, height) =>
      Stream
        .bracket(F.delay:
          var keepGoing = true

          val compiler = Compiler(canvas, width.toInt, height.toInt, config)

          def draw(t: Double): Unit =
            val setup = (
              dsl.save,
              dsl.clearRect(0, 0, width, height),
              dsl.marginTransform.flatMap(_.applyToCtx)
            ).tupled

            val cleanup = dsl.restore

            dispatcher.unsafeRunAndForget(
              getData.flatMap: data =>
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
    .compile
    .drain
    .background
    .void
