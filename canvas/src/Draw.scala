package ff4s.canvas

import cats.Monad
import cats.free.Free
import cats.free.Free.*

sealed trait DrawA[A]

object DrawA:
  case class Save() extends DrawA[Unit]
  case class Restore() extends DrawA[Unit]
  case class BeginPath() extends DrawA[Unit]
  case class Clip() extends DrawA[Unit]
  case class Fill() extends DrawA[Unit]
  case class SetFillStyle(color: Color) extends DrawA[Unit]
  case class Stroke() extends DrawA[Unit]
  case class SetStrokeStyle(color: Color) extends DrawA[Unit]
  case class ClearRect(x: Double, y: Double, w: Double, h: Double)
      extends DrawA[Unit]
  case class Arc(
      x: Double,
      y: Double,
      radius: Double,
      startAngle: Double,
      endAngle: Double,
      counterclockwise: Boolean
  ) extends DrawA[Unit]
  case class MoveTo(x: Double, y: Double) extends DrawA[Unit]
  case class LineTo(x: Double, y: Double) extends DrawA[Unit]
  case class IsPointInPath(x: Double, y: Double, fillRule: FillRule)
      extends DrawA[Boolean]
  case class SetLineWidth(width: Double) extends DrawA[Unit]
  case class SetFont(font: String) extends DrawA[Unit]
  case class SetTextAlign(align: TextAlign) extends DrawA[Unit]
  case class SetTextBaseline(baseline: TextBaseline) extends DrawA[Unit]
  case class FillText(
      text: String,
      x: Double,
      y: Double,
      maxWidth: Option[Double]
  ) extends DrawA[Unit]
  case class Translate(x: Double, y: Double) extends DrawA[Unit]
  case class Scale(x: Double, y: Double) extends DrawA[Unit]
  case class GetMousePos() extends DrawA[Point]
  case class GetTransform() extends DrawA[Transform]

type Draw[A] = Free[DrawA, A]

object dsl:
  import DrawA.*

  def pure[A](a: A): Draw[A] = Monad[Draw].pure(a)

  val save: Draw[Unit] = liftF[DrawA, Unit](Save())

  val restore: Draw[Unit] = liftF[DrawA, Unit](Restore())

  val beginPath: Draw[Unit] = liftF[DrawA, Unit](BeginPath())

  val clip: Draw[Unit] = liftF[DrawA, Unit](Clip())

  val fill: Draw[Unit] = liftF[DrawA, Unit](Fill())

  val stroke: Draw[Unit] = liftF[DrawA, Unit](Stroke())

  def fillStyle(color: Color): Draw[Unit] =
    liftF[DrawA, Unit](SetFillStyle(color))

  def strokeStyle(color: Color): Draw[Unit] =
    liftF[DrawA, Unit](SetStrokeStyle(color))

  def clearRect(x: Double, y: Double, w: Double, h: Double): Draw[Unit] =
    liftF[DrawA, Unit](ClearRect(x, y, w, h))

  def moveTo(x: Double, y: Double): Draw[Unit] =
    liftF[DrawA, Unit](MoveTo(x, y))

  def lineTo(x: Double, y: Double): Draw[Unit] =
    liftF[DrawA, Unit](LineTo(x, y))

  def isPointInPath(
      x: Double,
      y: Double,
      fillRule: FillRule
  ): Draw[Boolean] =
    liftF[DrawA, Boolean](IsPointInPath(x, y, fillRule))

  def arc(
      x: Double,
      y: Double,
      radius: Double,
      startAngle: Double,
      endAngle: Double,
      counterclockwise: Boolean
  ): Draw[Unit] =
    liftF[DrawA, Unit](
      Arc(x, y, radius, startAngle, endAngle, counterclockwise)
    )

  def lineWidth(width: Double): Draw[Unit] =
    liftF[DrawA, Unit](SetLineWidth(width))

  def font(font: String): Draw[Unit] =
    liftF[DrawA, Unit](SetFont(font))

  def textAlign(align: TextAlign): Draw[Unit] =
    liftF[DrawA, Unit](SetTextAlign(align))

  def textBaseline(baseline: TextBaseline): Draw[Unit] =
    liftF[DrawA, Unit](SetTextBaseline(baseline))

  def fillText(
      text: String,
      x: Double,
      y: Double,
      maxWidth: Option[Double] = None
  ): Draw[Unit] =
    liftF[DrawA, Unit](FillText(text, x, y, maxWidth))

  def translate(x: Double, y: Double): Draw[Unit] =
    liftF[DrawA, Unit](Translate(x, y))

  def scale(x: Double, y: Double): Draw[Unit] =
    liftF[DrawA, Unit](Scale(x, y))

  val mousePos: Draw[Point] = liftF[DrawA, Point](GetMousePos())

  val transform: Draw[Transform] = liftF[DrawA, Transform](GetTransform())
