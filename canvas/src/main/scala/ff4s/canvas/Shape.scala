package ff4s.canvas

import cats.syntax.all.*

enum Shape:

  case Circle(radius: Double, stroke: Option[Color], fill: Option[Color])

  case EquilateralTriangle(
      sideLength: Double,
      centered: Boolean,
      direction: Direction,
      stroke: Option[Color],
      fill: Option[Color]
  )

object Shape:

  private val SQRT_3 = math.sqrt(3)

  given Drawable[Shape] = new Drawable[Shape]:
    def draw(shape: Shape, at: Point): Draw[Unit] =
      import dsl.*
      shape match
        case Circle(radius, stroke, fill) =>
          for
            _ <- save
            _ <- beginPath
            _ <- arc(at.x, at.y, radius, 0, 2 * math.Pi, false)
            _ <- stroke.foldMapM(color => strokeStyle(color) *> dsl.stroke)
            _ <- fill.foldMapM(color => fillStyle(color) *> dsl.fill)
            _ <- restore
          yield ()

        case EquilateralTriangle(
              sideLength,
              centered,
              direction,
              stroke,
              fill
            ) =>
          val l = sideLength
          val h = l * SQRT_3 / 2
          val sign = direction match
            case Direction.Up   => 1
            case Direction.Down => -1

          val x1 = at.x
          val y1 = if centered then at.y - h / 2 else at.y
          val x2 = x1 - l / 2
          val y2 = y1 + sign * h
          val x3 = x1 + l / 2
          val y3 = y1 + sign * h
          for
            _ <- save
            _ <- beginPath
            _ <- moveTo(x1, y1)
            _ <- lineTo(x2, y2)
            _ <- lineTo(x3, y3)
            _ <- lineTo(x1, y1)
            _ <- stroke.foldMapM(color => strokeStyle(color) *> dsl.stroke)
            _ <- fill.foldMapM(color => fillStyle(color) *> dsl.fill)
            _ <- dsl.fill
            _ <- restore
          yield ()
