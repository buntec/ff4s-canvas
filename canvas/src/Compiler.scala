package ff4s.canvas

import cats.Id
import cats.syntax.all.*
import cats.~>
import org.scalajs.dom

private[canvas] object Compiler:

  def apply(
      canvas: dom.HTMLCanvasElement,
      initialWidth: Int,
      initialHeight: Int
  ): DrawA ~> Id =

    canvas.width = initialWidth
    canvas.height = initialHeight

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

    new ~>[DrawA, Id]:
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
