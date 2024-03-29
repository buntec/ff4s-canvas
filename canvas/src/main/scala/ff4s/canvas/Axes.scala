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

import cats.syntax.all.*

final case class Axes(
    width: Double,
    height: Double,
    xTickSize: Double,
    yTickSize: Double,
    xScale: Scale,
    yScale: Scale,
    nXTicks: Int,
    nYTicks: Int,
    tickFont: Font,
    axisColor: Color,
    textColor: Color,
    gridColor: Color,
    xTickFormat: Double => String = _.toString,
    yTickFormat: Double => String = _.toString,
    xTickFilter: Double => Boolean = _ => true,
    yTickFilter: Double => Boolean = _ => true
)

object Axes:

  given Drawable[Axes] = new Drawable[Axes]:
    import Draw.*
    def draw(a: Axes, at: Point): Draw[Unit] =
      val yTicks = Ticks
        .ticks(
          a.yScale.inverse(a.height - a.yTickSize),
          a.yScale.inverse(0.0),
          a.nYTicks
        )
        .filter(a.yTickFilter)

      val xTicks = Ticks
        .ticks(
          a.xScale.inverse(a.xTickSize),
          a.xScale.inverse(a.width),
          a.nXTicks
        )
        .filter(a.xTickFilter)

      for
        _ <- save
        _ <- translate(at.x, at.y)
        _ <- beginPath
        _ <- setLineWidth(1.0)
        _ <- setStrokeStyle(a.axisColor)
        _ <- moveTo(a.yTickSize, 0)
        _ <- lineTo(a.yTickSize, a.height)
        _ <- moveTo(0, a.height - a.xTickSize)
        _ <- lineTo(a.width, a.height - a.xTickSize)
        _ <- stroke
        _ <- beginPath

        _ <- setFillStyle(a.textColor)
        _ <- setFont(a.tickFont)

        // draw y-ticks
        _ <- setTextAlign(TextAlign.Right)
        _ <- yTicks.traverse_ : tick =>
          for
            _ <- moveTo(0, a.yScale(tick))
            _ <- lineTo(a.yTickSize, a.yScale(tick))
            _ <- setTextBaseline(TextBaseline.Middle)
            _ <- fillText(a.yTickFormat(tick), -a.yTickSize, a.yScale(tick))
          yield ()

        // draw x-ticks
        _ <- setTextAlign(TextAlign.Center)
        _ <- xTicks.traverse_ : tick =>
          for
            _ <- moveTo(a.xScale(tick), a.height - a.xTickSize)
            _ <- lineTo(a.xScale(tick), a.height)
            _ <- setTextBaseline(TextBaseline.Top)
            _ <- fillText(
              a.xTickFormat(tick),
              a.xScale(tick),
              a.height + a.xTickSize
            )
          yield ()

        _ <- stroke

        _ <- beginPath
        _ <- setStrokeStyle(a.gridColor)
        _ <- setFillStyle(a.gridColor)

        // draw horizontal grid lines
        _ <- yTicks.traverse_ : tick =>
          moveTo(a.yTickSize, a.yScale(tick)) *> lineTo(a.width, a.yScale(tick))

        // draw vertical grid lines
        _ <- xTicks.traverse_ : tick =>
          moveTo(a.xScale(tick), 0) *> lineTo(
            a.xScale(tick),
            a.height - a.xTickSize
          )

        _ <- stroke
        _ <- restore
      yield ()
