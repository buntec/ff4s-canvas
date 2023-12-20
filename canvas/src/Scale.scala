package ff4s.canvas

import cats.syntax.all.*

import Scale.*

trait Scale:

  def apply(x: Double): Double

  def inverse(y: Double): Double

  def range: Range

  def domain: Domain

  def withDomain(domain: Domain): Option[Scale]

  def withRange(range: Range): Option[Scale]

object Scale:

  case class Domain(min: Double, max: Double):
    def length = math.abs(max - min)

  case class Range(min: Double, max: Double):
    def length = math.abs(max - min)

  def linear(d: Domain, r: Range): Option[Scale] =
    (d.min != d.max && r.min != r.max)
      .guard[Option]
      .as(
        new Scale:
          def apply(x: Double): Double =
            r.min + (x - d.min) / d.length * r.length

          def inverse(y: Double): Double =
            d.min + (y - r.min) / r.length * d.length

          def range: Range = r

          def domain: Domain = d

          def withRange(range: Range): Option[Scale] = linear(d, range)

          def withDomain(domain: Domain): Option[Scale] = linear(domain, r)
      )
