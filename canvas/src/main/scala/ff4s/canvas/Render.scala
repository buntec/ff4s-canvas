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

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration.*

case class Settings(
    minZoom: Double = 0.2,
    maxZoom: Double = 5.0,
    zoomSensitivity: Double = 0.001,
    relMargin: Double = 0.01,
    transitionDuration: FiniteDuration = 500.millis,
    transitionEasing: Easing = Easing.CubicInOut
)

def loop[F[_]: Dom, D: Eq: Transition](
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
          var handle: Option[Int] = None

          val compiler = Compiler(
            canvas.asInstanceOf[dom.HTMLCanvasElement],
            width.toInt,
            height.toInt,
            config
          )

          def draw(t: Double): Unit =
            val setup = (
              Draw.save,
              Draw.clearRect(0, 0, width, height),
              Draw.marginTransform.flatMap(_.applyToCtx)
            ).tupled

            val cleanup = Draw.restore

            dispatcher.unsafeRunAndForget(
              (F.realTime, currentAndPrevData.get).flatMapN {
                case (d0, (data, d, dataPrev, dPrev)) =>
                  val u0 = (d0 - d) / config.transitionDuration
                  val u = config.transitionEasing(u0)
                  val data0 = Transition[D](dataPrev, data, u)
                  F.delay:
                    (setup *> drawFrame(
                      DOMHighResTimeStamp(t),
                      data0
                    ) *> cleanup)
                      .foldMap(compiler)
                    if (keepGoing) {
                      handle = Some(dom.window.requestAnimationFrame(draw _))
                    }
              }
            )

          draw(0)

          F.delay:
            keepGoing = false
            handle.foreach(dom.window.cancelAnimationFrame(_))
        )(identity)
        .evalMap(_ => F.never)
    }
    .compile
    .drain
    .background

} yield ()
