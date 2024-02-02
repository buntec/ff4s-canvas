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

import cats.Id
import cats.syntax.all.*
import cats.~>
import org.scalajs.dom

import scalajs.js

private[canvas] object Compiler:

  def apply(
      canvas: dom.HTMLCanvasElement,
      cssPixelWidth: Int,
      cssPixelHeight: Int,
      settings: render.Settings
  ): DrawA ~> Id =
    val kvs = collection.mutable.Map.empty[String, Any]

    val ctx: dom.CanvasRenderingContext2D =
      canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    val width = cssPixelWidth // should be equal to canvas.style.width in px
    val height = cssPixelHeight // should be equal to canvas.style.height in px

    val devicePixelRatio = dom.window.devicePixelRatio
    canvas.width = (devicePixelRatio * width).toInt
    canvas.height = (devicePixelRatio * height).toInt

    val marginTop = height * settings.relMargin
    val marginBottom = height * settings.relMargin
    val marginLeft = width * settings.relMargin
    val marginRight = width * settings.relMargin

    val effectiveWidth = (width - marginLeft - marginRight).toInt
    val effectiveHeight = (height - marginTop - marginBottom).toInt

    val marginTransform =
      Transform.translate(marginLeft, marginTop) andThen Transform.scale(
        devicePixelRatio
      )

    var transform = Transform.identity
    var mousePos = Point(0, 0)
    var mouseDown = false
    var dragStartPos = Point(0, 0)
    var transformOnDragStart = transform

    def onMouseDown(ev: dom.MouseEvent): Unit =
      mouseDown = true
      transformOnDragStart = transform
      dragStartPos = mouse.getPos(ev, ctx)

    def onMouseUp(): Unit =
      mouseDown = false

    def onMouseMove(ev: dom.MouseEvent) =
      mousePos = mouse.getPos(ev, ctx)
      val p1 = marginTransform.inverse(dragStartPos)
      val p2 = marginTransform.inverse(mousePos)
      if (mouseDown) {
        val deltaX = p2.x - p1.x
        val deltaY = p2.y - p1.y
        transform =
          transformOnDragStart andThen Transform.translate(deltaX, deltaY)
      }

    canvas.addEventListener(
      "mousedown",
      (ev: dom.Event) => onMouseDown(ev.asInstanceOf[dom.MouseEvent])
    )

    canvas.addEventListener("mouseup", (_: dom.Event) => onMouseUp())

    canvas.addEventListener(
      "mousemove",
      (ev: dom.Event) => onMouseMove(ev.asInstanceOf[dom.MouseEvent])
    )

    canvas.addEventListener(
      "wheel",
      (ev: dom.Event) =>
        ev.preventDefault()
        if (!mouseDown) {
          val wev = ev.asInstanceOf[dom.WheelEvent]
          val p = marginTransform.inverse(mouse.getPos(wev, ctx))
          val scroll = -1 * wev.deltaY * settings.zoomSensitivity
          if (
            scroll > 0 && transform.k < settings.maxZoom ||
            scroll < 0 && transform.k > settings.minZoom
          ) {
            transform = transform
              .andThen(Transform.translate(-p.x, -p.y))
              .andThen(Transform.scale(math.exp(scroll)))
              .andThen(Transform.translate(p.x, p.y))
          }
        }
    )

    new ~>[DrawA, Id]:
      def apply[A](da: DrawA[A]): Id[A] =
        import DrawA.*
        da match
          // custom
          case GetWidth()           => effectiveWidth
          case GetHeight()          => effectiveHeight
          case GetTransform()       => transform
          case GetMousePos()        => mousePos
          case GetMarginTransform() => marginTransform
          case Clear() =>
            ctx.save()
            ctx.setTransform(1, 0, 0, 1, 0, 0) // identity
            ctx.clearRect(0, 0, canvas.width, canvas.height)
            ctx.restore()

          // kv
          case KVPut(key, value) =>
            kvs(key) = value

          case KVGet(key) =>
            kvs.get(key).asInstanceOf[A]

          case KVDelete(key) =>
            kvs.remove(key)
            ()

          // canvas ctx
          case Save() => ctx.save()

          case MeasureText(text) =>
            ctx.measureText(text)

          case Restore() => ctx.restore()

          case BeginPath() => ctx.beginPath()
          case ClosePath() => ctx.closePath()

          case Clip() => ctx.clip()

          case Fill() => ctx.fill()

          case Fill2(path) =>
            val p2d = Path.toPath2D(path)
            ctx.fill(p2d)

          case SetGlobalAlpha(alpha) =>
            ctx.globalAlpha = alpha

          case SetShadowBlur(blue) =>
            ctx.shadowBlur = blue

          case SetShadowColor(color) =>
            ctx.shadowColor = color.toString

          case SetShadowOffsetX(offset) =>
            ctx.shadowOffsetX = offset

          case SetShadowOffsetY(offset) =>
            ctx.shadowOffsetY = offset

          case SetFillStyle(color) =>
            ctx.fillStyle = color.toString

          case Stroke() => ctx.stroke()

          case SetStrokeStyle(color) =>
            ctx.strokeStyle = color.toString

          case ClearRect(x, y, w, h) => ctx.clearRect(x, y, w, h)

          case FillRect(x, y, w, h) => ctx.fillRect(x, y, w, h)

          case Arc(x, y, radius, startAngle, endAngle, counterclockwise) =>
            ctx.arc(x, y, radius, startAngle, endAngle, counterclockwise)

          case Rect(x, y, width, height) => ctx.rect(x, y, width, height)

          case RoundRect(x, y, width, height, radii) =>
            ctx
              .asInstanceOf[js.Dynamic]
              .roundRect(x, y, width, height, radii)
            ()

          case MoveTo(x, y) => ctx.moveTo(x, y)
          case LineTo(x, y) => ctx.lineTo(x, y)

          case IsPointInPath(x, y, fillRule) =>
            val domFillRule = fillRule match
              case FillRule.Nonzero => dom.CanvasFillRule.nonzero
              case FillRule.EvenOdd => dom.CanvasFillRule.evenodd
            ctx.isPointInPath(x, y, domFillRule)

          case IsPointInPath2(path, x, y, fillRule) =>
            val domFillRule = fillRule match
              case FillRule.Nonzero => dom.CanvasFillRule.nonzero
              case FillRule.EvenOdd => dom.CanvasFillRule.evenodd
            val p2d = Path.toPath2D(path)
            ctx.isPointInPath(p2d, x, y, domFillRule)

          case SetLineWidth(width) =>
            ctx.lineWidth = width

          case SetLineDash(pattern) =>
            ctx.setLineDash(js.Array(pattern*))

          case SetFont(font) =>
            ctx.font = font.show

          case SetTextAlign(align) =>
            ctx.textAlign = align match
              case TextAlign.Start  => "start"
              case TextAlign.End    => "end"
              case TextAlign.Left   => "left"
              case TextAlign.Right  => "right"
              case TextAlign.Center => "center"

          case SetTextBaseline(baseline) =>
            ctx.textBaseline = baseline match
              case TextBaseline.Top         => "top"
              case TextBaseline.Middle      => "middle"
              case TextBaseline.Bottom      => "bottom"
              case TextBaseline.Hanging     => "hanging"
              case TextBaseline.Ideographic => "ideographic"
              case TextBaseline.Alphabetic  => "alphabetic"

          case SetDirection(dir) => ??? // TODO: missing from org.scalajs.dom ?

          case FillText(text, x, y, maxWidth) =>
            ctx.fillText(text, x, y)

          case Translate(x, y) => ctx.translate(x, y)

          case Scale(x, y) => ctx.scale(x, y)
