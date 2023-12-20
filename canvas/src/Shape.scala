package ff4s.canvas

import org.scalajs.dom.CanvasRenderingContext2D

enum Direction:
  case Up, Down

enum Shape:

  case Circle(radius: Double, stroke: Option[Color], fill: Option[Color])

  case EquitlateralTriangle(
      sideLength: Double,
      centered: Boolean,
      direction: Direction,
      stroke: Option[Color],
      fill: Option[Color]
  )

object Shape:

  given Drawable[Shape] = new Drawable[Shape]:
    def draw(shape: Shape, at: Point)(using
        ctx: CanvasRenderingContext2D
    ): Unit =
      shape match
        case Circle(radius, stroke, fill) =>
          ctx.save()
          ctx.beginPath()
          ctx.arc(at.x, at.y, radius, 0, 2 * math.Pi, false)
          stroke.foreach: color =>
            ctx.strokeStyle = color.toString
            ctx.stroke()
          fill.foreach: color =>
            ctx.fillStyle = color.toString
            ctx.fill()
          ctx.restore()

        case EquitlateralTriangle(
              sideLength,
              centered,
              direction,
              stroke,
              fill
            ) =>
          val l = sideLength
          val h = l * math.sqrt(3) / 2
          val sign = direction match
            case Direction.Up   => 1
            case Direction.Down => -1

          val x1 = at.x
          val y1 = if centered then at.y - h / 2 else at.y
          val x2 = x1 - l / 2
          val y2 = y1 + sign * h
          val x3 = x1 + l / 2
          val y3 = y1 + sign * h
          ctx.save()
          ctx.beginPath()
          ctx.moveTo(x1, y1)
          ctx.lineTo(x2, y2)
          ctx.lineTo(x3, y3)
          ctx.lineTo(x1, y1)
          stroke.foreach: color =>
            ctx.strokeStyle = color.toString
            ctx.stroke()
          fill.foreach: color =>
            ctx.fillStyle = color.toString
            ctx.fill()
          ctx.restore()
