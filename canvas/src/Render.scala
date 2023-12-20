package ff4s.canvas
package render

import cats.syntax.all.*
import cats.~>
import cats.effect.*
import cats.effect.implicits.*

import org.scalajs.dom
import cats.effect.std.Dispatcher
import fs2.Stream
import cats.Id
import fs2.concurrent.SignallingRef
import fs2.dom.ResizeObserver

def render[F[_], D](
    canvas: dom.HTMLCanvasElement,
    dispatcher: Dispatcher[F],
    getData: F[D],
    drawFrame: (Double, D) => Draw[Unit] // (time, data)
)(using F: Async[F]) =
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
              canvas.width = width0.toInt
              canvas.height = height0.toInt

              var keepGoing = true

              val zoomSensitivity = 0.002
              val minZoom = 0.2
              val maxZoom = 5

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

              given ctx: dom.CanvasRenderingContext2D =
                canvas
                  .getContext("2d")
                  .asInstanceOf[dom.CanvasRenderingContext2D]

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
                  transform = transformOnDragStart.andThen(
                    Transform.translate(deltaX, deltaY)
                  )
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

              def compiler: DrawA ~> Id = new ~>[DrawA, Id]:
                def apply[A](fa: DrawA[A]): Id[A] = {
                  import DrawA.*
                  fa match
                    case GetTransform() => transform
                    case GetMousePos()  => mousePos
                    case Save()         => ctx.save()
                    case Restore()      => ctx.restore()
                    case BeginPath()    => ctx.beginPath()
                    case Clip()         => ctx.clip()
                    case Fill()         => ctx.fill()
                    case SetFillStyle(color) => {
                      ctx.fillStyle = color.toString
                    }
                    case Stroke() => ctx.stroke()
                    case SetStrokeStyle(color) => {
                      ctx.strokeStyle = color.toString
                    }
                    case ClearRect(x, y, w, h) => ctx.clearRect(x, y, w, h)
                    case Arc(
                          x,
                          y,
                          radius,
                          startAngle,
                          endAngle,
                          counterclockwise
                        ) =>
                      ctx.arc(
                        x,
                        y,
                        radius,
                        startAngle,
                        endAngle,
                        counterclockwise
                      )
                    case MoveTo(x, y) => ctx.moveTo(x, y)
                    case LineTo(x, y) => ctx.lineTo(x, y)
                    case IsPointInPath(x, y, fillRule) =>
                      ctx.isPointInPath(x, y)
                    case SetLineWidth(width) => {
                      ctx.lineWidth = width
                    }
                    case SetFont(font) => {
                      ctx.font = font
                    }
                    case SetTextAlign(align) => {
                      ctx.textAlign = align.toString
                    }
                    case SetTextBaseline(baseline) => {
                      ctx.textBaseline = baseline.toString
                    }
                    case FillText(text, x, y, maxWidth) => {
                      ctx.fillText(text, x, y)
                    }
                    case Translate(x, y) => ctx.translate(x, y)
                    case Scale(x, y)     => ctx.scale(x, y)

                }

              def draw(t: Double): Unit =

                val c = compiler

                val setup = (
                  dsl.save,
                  dsl.clearRect(0, 0, width.toDouble, heigh.toDouble),
                  marginTransform.applyToCtx
                ).tupled

                val cleanup = dsl.restore

                dispatcher.unsafeRunAndForget(
                  getData.flatMap: data =>
                    F.delay:
                      setup.foldMap(c)
                      drawFrame(t, data).foldMap(c)
                      cleanup.foldMap(c)
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
