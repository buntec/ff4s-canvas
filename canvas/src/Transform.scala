package ff4s.canvas

import cats.syntax.all.*

case class Transform(x: Double, y: Double, k: Double):

  def applyX(x: Double): Double = k * x + this.x
  def applyY(y: Double): Double = k * y + this.y
  def apply(point: Point): Point = Point(applyX(point.x), applyY(point.y))

  def invertX(x: Double): Double = (x - this.x) / k
  def invertY(y: Double): Double = (y - this.y) / k
  def invert(point: Point): Point = Point(invertX(point.x), invertY(point.y))

  def after(t: Transform): Transform =
    Transform(t.x * k + x, t.y * k + y, t.k * k)

  def andThen(t: Transform): Transform = t.after(this)

  def applyToCtx: Draw[Unit] = dsl.translate(x, y) *> dsl.scale(k, k)

  def rescaleX(scale: Scale): Option[Scale] =
    val range = Scale.Range(invertX(scale.range.min), invertX(scale.range.max))
    val domain =
      Scale.Domain(scale.inverse(range.min), scale.inverse(range.max))
    scale.withDomain(domain)

  def rescaleY(scale: Scale): Option[Scale] =
    val range = Scale.Range(invertY(scale.range.min), invertY(scale.range.max))
    val domain =
      Scale.Domain(scale.inverse(range.min), scale.inverse(range.max))
    scale.withDomain(domain)

  def inverse: Transform = Transform(-x / k, -y / k, 1 / k)

object Transform:

  def scale(k: Double): Transform = Transform(0, 0, k)

  def translate(x: Double, y: Double): Transform = Transform(x, y, 1)

  def identity: Transform = Transform(0, 0, 1)

  def none: Transform = identity
