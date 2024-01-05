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
import cats.Monad
import scalajs.js

object ScatterPlot:

  final case class Config(
      nXTicks: Int,
      nYTicks: Int,
      tickFont: Font,
      axisColor: Color,
      textColor: Color,
      gridColor: Color,
      legend: Boolean
  )

  final case class Trace(
      points: List[Point],
      marker: Marker,
      label: Option[String]
  )

  object Trace:
    given Eq[Trace] = Eq.fromUniversalEquals
    given Transition[Trace] = Transition.transition((tr1, tr2, t) =>
      Trace(
        Transition[List[Point]](tr1.points, tr2.points, t),
        Transition[Marker](tr1.marker, tr2.marker, t),
        tr2.label
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
        _ <- setFillStyle(config.textColor)
        _ <- setFont(config.tickFont)
        _ <- fillText(
          f"x=${hover.x}%.2f, y=${hover.y}%.2f",
          mp.x + 0.02 * w,
          mp.y - 0.02 * h
        )
        _ <- restore
      yield ()

    def legend(traces: List[Trace]): Draw[Unit] =
      import Draw.*
      for
        _ <- save
        w <- width
        h <- height
        dy = 0.05 * h
        dx = 0.04 * w
        _ <- translate(w - 4 * dx, dy)
        _ <- setFont(config.tickFont)
        n = traces.length
        maxTextWidth <- traces
          .map(_.label.getOrElse("?"))
          .traverse(s => measureText(s))
          .map(_.map(_.width).max)

        _ <- save
        _ <- setGlobalAlpha(0.5)
        _ <- setFillStyle(Color.Black)
        // _ <- fillRect(-dx / 2, 0, maxTextWidth * 1.4, (n + 1) * dy)
        _ <- roundRect(-dx / 2, 0, maxTextWidth * 1.4, (n + 1) * dy, 6)
        _ <- fill
        _ <- restore

        _ <- setFillStyle(config.textColor)
        _ <- traces.zipWithIndex.traverse: (trace, i) =>
          val y = (i + 1) * dy
          val text = trace.label.getOrElse("?")
          for
            metric <- measureText(text)
            bbt = metric
              .asInstanceOf[js.Dynamic]
              .actualBoundingBoxAscent
              .asInstanceOf[Double]
            bbb = metric
              .asInstanceOf[js.Dynamic]
              .actualBoundingBoxDescent
              .asInstanceOf[Double]
            adj = (bbt - bbb) / 2
            _ <- fillText(
              text,
              trace.marker.toSize,
              y + adj
            )
            _ <- trace.marker.draw(Point(0, y))
          yield ()
        _ <- restore
      yield ()

    val drawFrame = (t: DOMHighResTimeStamp, traces: List[Trace]) =>
      import Draw.*
      val nPoints = traces.map(_.points.length).sum
      Monad[Draw].whenA(nPoints > 0):
        val xMin = traces.flatMap(_.points.map(_.x)).min
        val xMax = traces.flatMap(_.points.map(_.x)).max
        val yMin = traces.flatMap(_.points.map(_.y)).min
        val yMax = traces.flatMap(_.points.map(_.y)).max
        for
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
                  for
                    _ <- kvPut("hover", point)
                    _ <- save
                    _ <- setShadowColor(c0)
                    _ <- setShadowBlur(trace.marker.toSize / 2)
                    _ <- marker.draw(at)
                    _ <- restore
                  yield ()
                else trace.marker.draw(at)
            }
          }
          _ <- restore
          _ <- kvGet[Point]("hover").flatMap(_.foldMapM(tooltip))
          _ <- Monad[Draw].whenA(config.legend)(legend(traces))
        yield ()

    ff4s.canvas.render.loop(
      elm,
      dispatcher,
      traces,
      drawFrame,
      ff4s.canvas.render.Settings(
        relMargin = 0.05
      )
    )
