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

package examples

import cats.effect.kernel.Async
import cats.effect.kernel.Resource
import cats.effect.std.Dispatcher
import cats.kernel.Eq
import cats.syntax.all.*
import ff4s.canvas.syntax.*
import fs2.concurrent.Signal
import fs2.dom.Dom
import fs2.dom.HtmlCanvasElement
import ff4s.canvas.*
import org.scalajs.dom.MouseEvent

object VolSkewChart:

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

  final case class DrawResult(
      mt: Transform,
      mousePosCalc: MouseEvent => Point,
      xScale: Scale,
      yScale: Scale,
      hoveredPoint: Option[Point]
  )

  def apply[F[_]: Dom](
      config: Config,
      trace: Signal[F, Trace],
      elm: HtmlCanvasElement[F],
      dispatcher: Dispatcher[F]
  )(using F: Async[F]): Resource[F, Signal[F, Option[DrawResult]]] =

    def tooltip(hover: Point): Draw[Unit] =
      import Draw.*
      for
        _ <- save
        w <- width
        h <- height
        mp0 <- mousePos
        mt <- marginTransform
        mp = mt.invert(mp0)
        _ <- setFillStyle(config.textColor)
        _ <- setFont(config.tickFont)
        _ <- fillText(
          f"x=${hover.x}%.2f, y=${hover.y}%.2f",
          mp.x + 0.02 * w,
          mp.y - 0.02 * h
        )
        _ <- restore
      yield ()

    val drawFrame = (t: DOMHighResTimeStamp, trace: Trace) =>
      import Draw.*
      val nPoints = trace.points.length
      if nPoints > 0 then
        val xMin = trace.points.map(_.x).min
        val xMax = trace.points.map(_.x).max
        val yMin = trace.points.map(_.y).min
        val yMax = trace.points.map(_.y).max
        for
          _ <- save
          w <- width
          h <- height
          xScale0 = Scale
            .linear(
              Scale.Domain(xMin, xMax),
              Scale.Range(0, w)
            )
            .getOrElse(throw new Exception("failed to build x-scale"))
          yScale0 = Scale
            .linear(
              Scale.Domain(yMin, yMax),
              Scale.Range(h, 0)
            )
            .getOrElse(throw new Exception("failed to build y-scale"))
          trans <- transform
          xScale = trans
            .rescaleX(xScale0)
            .getOrElse(throw new Exception("failed to rescale x-scale"))
          yScale = trans
            .rescaleY(yScale0)
            .getOrElse(throw new Exception("failed to rescale y-scale"))
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
          _ <-
            trace.points.traverse_ : point =>
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
                  for
                    _ <- kvPut("hover", point)
                    _ <- save
                    _ <- setShadowColor(c0)
                    _ <- setShadowBlur(trace.marker.toSize / 2)
                    _ <- marker.draw(at)
                    _ <- restore
                  yield ()
                else trace.marker.draw(at)

          _ <- restore
          hoveredPoint <- kvGet[Point]("hover")
          _ <- hoveredPoint.foldMapM(tooltip)
          mt <- marginTransform
          mousePosCalc <- mousePosCalc
        yield Some(DrawResult(mt, mousePosCalc, xScale, yScale, hoveredPoint))
      else Draw.pure(Option.empty[DrawResult])

    render.loop(
      elm,
      dispatcher,
      trace,
      drawFrame,
      render.Settings(
        margins = Margins.uniformRelative(0.05),
        disableDrag = true
      )
    )
