package ff4s.canvas
package render

import cats.syntax.all.*
import cats.effect.*
import cats.effect.implicits.*

import org.scalajs.dom
import cats.effect.std.Dispatcher
import fs2.Stream

def render[F[_]](
    canvas: dom.HTMLCanvasElement,
    dispatcher: Dispatcher[F],
    redraw: (Double, dom.CanvasRenderingContext2D) => F[Unit]
)(using F: Async[F]) =
  Stream
    .bracket(F.delay:
      var keepGoing = true

      val zoomSensitivity = 0.1
      val maxZoom = 4
      val minZoom = 0.25

      val width = canvas.width
      val heigh = canvas.height
      val marginLeft = 0.025 * width
      val marginTop = 0.025 * heigh
      val marginTransform = Transform.translate(marginLeft, marginTop)

      var transform = Transform.identity
      var mousePos = Point(0, 0)
      var mouseDown = false
      var dragStartPos = Point(0, 0)
      var transformOnDragStart = transform
      var tooltip = ""

      given ctx: dom.CanvasRenderingContext2D =
        canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

      def onMouseDown(ev: dom.MouseEvent): Unit =
        mouseDown = true
        transformOnDragStart = transform
        dragStartPos = mouse.getPos(ev)

      def onMouseUp(): Unit =
        mouseDown = false

      def onMouseMove(ev: dom.MouseEvent) =
        mousePos = mouse.getPos(ev)
        if (mouseDown) {
          val deltaX = mousePos.x - dragStartPos.x
          val deltaY = mousePos.y - dragStartPos.y
          transform =
            transformOnDragStart.andThen(Transform.translate(deltaX, deltaY))
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
            val p = marginTransform.invert(mouse.getPos(wev))
            val scroll = -1 * wev.deltaY * zoomSensitivity
            if (
              scroll > 0 && transform.k < maxZoom ||
              scroll < 0 && transform.k > minZoom
            ) {
              transform = transform
                .andThen(Transform.translate(-p.x, -p.y))
                .andThen(Transform.scale(math.exp(scroll)))
                .andThen(Transform.translate(p.x, p.y))
            }
          }
      )

      def draw(t: Double): Unit =
        dispatcher.unsafeRunAndForget(
          F.delay {
            ctx.save()
            ctx.clearRect(0, 0, width.toDouble, heigh.toDouble)
            marginTransform.applyToCtx(ctx)
          } *> redraw(t, ctx)
            *> F.delay {
              ctx.restore()
              if (keepGoing) {
                dom.window.requestAnimationFrame(draw _): Unit
              }
            }
        )

      draw(0)

      F.delay:
        keepGoing = false
    )(identity)
    .evalMap(_ => F.never[Unit])
    .compile
    .drain
    .background
    .void
