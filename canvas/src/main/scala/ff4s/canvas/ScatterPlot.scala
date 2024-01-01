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

import cats.effect.kernel.Async
import cats.effect.kernel.Resource
import cats.effect.std.Dispatcher
import cats.kernel.Eq
import cats.syntax.all.*
import ff4s.canvas.syntax.*
import fs2.concurrent.Signal
import fs2.dom.Dom
import fs2.dom.HtmlCanvasElement

object ScatterPlot:

  final case class Trace(
      points: List[Point],
      color: Color
  )

  object Trace:
    given Eq[Trace] = Eq.fromUniversalEquals
    given Transition[Trace] = Transition.transition((tr1, tr2, t) =>
      Trace(Transition[List[Point]](tr1.points, tr2.points, t), tr2.color)
    )

  def apply[F[_]: Dom](
      traces: Signal[F, List[Trace]],
      elm: HtmlCanvasElement[F],
      dispatcher: Dispatcher[F]
  )(using F: Async[F]): Resource[F, Unit] =

    val drawFrame = (t: DOMHighResTimeStamp, traces: List[Trace]) => {
      import dsl.*
      val m = t.toMillis / 1000
      val nPoints = traces.map(_.points.length).sum
      if nPoints > 0 then
        val xMin = traces.flatMap(_.points.map(_.x)).min
        val xMax = traces.flatMap(_.points.map(_.x)).max
        val yMin = traces.flatMap(_.points.map(_.y)).min
        val yMax = traces.flatMap(_.points.map(_.y)).max
        for {
          _ <- save
          w <- width
          h <- height
          xScale0 = Scale
            .linear(
              Scale.Domain(xMin, xMax),
              Scale.Range(0, w)
            )
            .get
          yScale0 = Scale
            .linear(
              Scale.Domain(yMin, yMax),
              Scale.Range(h, 0)
            )
            .get
          trans <- transform
          xScale = trans.rescaleX(xScale0).get
          yScale = trans.rescaleY(yScale0).get
          xTickSize = 0.01 * h
          yTickSize = 0.01 * w
          _ <- Axes(
            w,
            h,
            xTickSize,
            yTickSize,
            xScale,
            yScale,
            20,
            20,
            "normal 100 12px system-ui",
            Color.Keyword("gray"),
            Color.Keyword("black"),
            Color.Keyword("silver")
          ).draw(Point(0, 0))

          // clip everything outside axes region
          _ <- beginPath
          _ <- rect(yTickSize, 0, w - yTickSize, h - xTickSize)
          _ <- clip

          _ <- traces.traverse_ { trace =>
            trace.points.traverse_ { point =>
              val point0 = Point(xScale(point.x), yScale(point.y))
              (Shape.Circle(10, trace.color.some, None): Shape).draw(point0)
            }
          }
          _ <- restore
        } yield ()
      else dsl.noop
    }

    ff4s.canvas.render.loop(
      elm,
      dispatcher,
      traces,
      drawFrame,
      ff4s.canvas.render.Settings(
        relMargin = 0.05
      )
    )