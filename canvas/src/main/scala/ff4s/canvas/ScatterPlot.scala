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

  final case class Config(
      nXTicks: Int,
      nYTicks: Int,
      tickFont: Font,
      axisColor: Color,
      textColor: Color,
      gridColor: Color
  )

  final case class Trace(
      points: List[Point],
      marker: Marker
  )

  object Trace:
    given Eq[Trace] = Eq.fromUniversalEquals
    given Transition[Trace] = Transition.transition((tr1, tr2, t) =>
      Trace(
        Transition[List[Point]](tr1.points, tr2.points, t),
        Transition[Marker](tr1.marker, tr2.marker, t)
      )
    )

  def apply[F[_]: Dom](
      config: Config,
      traces: Signal[F, List[Trace]],
      elm: HtmlCanvasElement[F],
      dispatcher: Dispatcher[F]
  )(using F: Async[F]): Resource[F, Unit] =

    def tooltip(hover: Point): Draw[Unit] =
      import Draw.*
      for
        _ <- save
        w <- width
        h <- height
        mp0 <- mousePos
        mt <- marginTransform
        mp = mt.invert(mp0)
        // _ <- fillStyle(Color.Black)
        // _ <- fillRect(mp.x + 0.02 * w, mp.y - 0.05 * h, 0.05 * w, 0.05 * h)
        _ <- fillStyle(config.textColor)
        _ <- font(config.tickFont)
        _ <- fillText(
          f"x=${hover.x}%.2f, y=${hover.y}%.2f",
          mp.x + 0.02 * w,
          mp.y - 0.02 * h
        )
        _ <- restore
      yield ()

    val drawFrame = (t: DOMHighResTimeStamp, traces: List[Trace]) => {
      import Draw.*
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
            config.nXTicks,
            config.nYTicks,
            config.tickFont,
            config.axisColor,
            config.textColor,
            config.gridColor
          ).draw(Point(0, 0))

          // clip everything outside axes region
          _ <- beginPath
          _ <- rect(yTickSize, 0, w - yTickSize, h - xTickSize)
          _ <- clip

          mp <- mousePos
          _ <- kvDelete("hover") // reset hover info
          _ <- traces.traverse_ { trace =>
            trace.points.traverse_ { point =>
              val at = Point(xScale(point.x), yScale(point.y))
              val boundingPath = trace.marker.boundingPath(at)
              isPointInPath(
                boundingPath,
                mp.x,
                mp.y,
                FillRule.Nonzero
              ).flatMap: isHover =>
                if isHover then
                  val c0 = trace.marker.toColor
                  val marker = trace.marker.withColor(c0.lighten(20))
                  kvPut("hover", point) *> marker.draw(at)
                else trace.marker.draw(at)

            }
          }
          _ <- restore

          _ <- kvGet[Point]("hover").flatMap(_.foldMapM(tooltip))

        } yield ()
      else Draw.noop
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
