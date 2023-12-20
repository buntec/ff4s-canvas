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

def loop[F[_], D](
    canvas: dom.HTMLCanvasElement,
    dispatcher: Dispatcher[F],
    getData: F[D],
    drawFrame: (Double, D) => Draw[Unit] // (time, data)
)(using F: Async[F]): Resource[F, Unit] =
  Stream
    .eval(
      F.delay((canvas.offsetWidth, canvas.offsetHeight))
        .flatMap(
          SignallingRef.of[F, (Double, Double)](_)
        )
    )
    .flatMap: sizeRef =>
      Stream
        .resource(
          ResizeObserver[F]((a, _) =>
            sizeRef.set((a.head.contentRect.width, a.head.contentRect.height))
          ).evalTap(_.observe(canvas.asInstanceOf[fs2.dom.Element[F]]))
            .as(sizeRef)
        )
        .flatMap(_.discrete)
        .switchMap { case (width0, height0) =>
          Stream
            .bracket(F.delay:
              var keepGoing = true

              val compiler = Compiler(canvas, width0.toInt, height0.toInt)

              def draw(t: Double): Unit =

                val setup = (
                  dsl.save,
                  dsl.clearRect(0, 0, width0, height0),
                  // marginTransform.applyToCtx
                ).tupled

                val cleanup = dsl.restore

                dispatcher.unsafeRunAndForget(
                  getData.flatMap: data =>
                    F.delay:
                      setup.foldMap(compiler)
                      drawFrame(t, data).foldMap(compiler)
                      cleanup.foldMap(compiler)
                      if (keepGoing) {
                        dom.window.requestAnimationFrame(draw _): Unit
                      }
                )

              draw(0)

              F.delay:
                keepGoing = false
            )(identity)
            .evalMap(_ => F.never[Unit])
        }
    .compile
    .drain
    .background
    .void
