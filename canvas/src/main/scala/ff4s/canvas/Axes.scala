package ff4s.canvas

import cats.syntax.all.*

final case class Axes(
    width: Double,
    height: Double,
    tickSize: Double,
    xScale: Scale,
    yScale: Scale,
    nXTicks: Int,
    nYTicks: Int,
    tickFont: String,
    axisColor: Color,
    textColor: Color,
    gridColor: Color
)

object Axes:

  given Drawable[Axes] = new Drawable[Axes]:
    import dsl.*
    def draw(a: Axes, at: Point): Draw[Unit] =
      val yTicks = Ticks
        .ticks(
          a.yScale.inverse(a.height - a.tickSize),
          a.yScale.inverse(0.0),
          a.nYTicks
        )
        .filter(_ >= 0)
      val xTicks = Ticks
        .ticks(
          a.xScale.inverse(a.tickSize),
          a.xScale.inverse(a.width),
          a.nXTicks
        )
        .filter(_ >= 0)
      for
        _ <- save
        _ <- beginPath
        _ <- lineWidth(1.0)
        _ <- strokeStyle(a.axisColor)
        _ <- moveTo(a.tickSize, 0)
        _ <- lineTo(a.tickSize, a.height)
        _ <- moveTo(0, a.height - a.tickSize)
        _ <- lineTo(a.width, a.height - a.tickSize)
        _ <- stroke
        _ <- beginPath
        _ <- fillStyle(a.textColor)
        _ <- strokeStyle(a.axisColor)

        _ <- font(a.tickFont)
        _ <- textAlign(TextAlign.Right)
        _ <- yTicks.traverse_ { tick =>
          for
            _ <- moveTo(0, a.yScale(tick))
            _ <- lineTo(a.tickSize, a.yScale(tick))
            _ <- textBaseline(TextBaseline.Middle)
            _ <- fillText(tick.toString, -a.tickSize, a.yScale(tick))
          yield ()
        }

        _ <- textAlign(TextAlign.Center)
        _ <- xTicks.traverse_ { tick =>
          for
            _ <- moveTo(a.xScale(tick), a.height - a.tickSize)
            _ <- lineTo(a.xScale(tick), a.height)
            _ <- textBaseline(TextBaseline.Top)
            _ <- fillText(tick.toString, a.xScale(tick), a.height + a.tickSize)
          yield ()
        }

        _ <- stroke

        _ <- beginPath
        _ <- strokeStyle(a.gridColor)
        _ <- fillStyle(a.gridColor)

        _ <- yTicks.traverse_ { tick =>
          moveTo(a.tickSize, a.yScale(tick)) *> lineTo(a.width, a.yScale(tick))
        }

        _ <- xTicks.traverse_ { tick =>
          moveTo(a.xScale(tick), 0) *> lineTo(
            a.xScale(tick),
            a.height - a.tickSize
          )
        }
        _ <- stroke
        _ <- restore
      yield ()
