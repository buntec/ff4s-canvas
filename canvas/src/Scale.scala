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
    (d.length != 0 && r.length != 0)
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

  def sqrt(d: Domain, r: Range): Option[Scale] =
    (d.length != 0 && r.length != 0)
      .guard[Option]
      .as(new Scale:
        def range: Range = r

        def domain: Domain = d

        def apply(x: Double): Double = r.min + r.length * (math.sqrt(x) - math
          .sqrt(d.min)) / (math.sqrt(d.max) - math.sqrt(d.min))

        def inverse(y: Double): Double =
          val sqrtX = math.sqrt(d.min) + (y - r.min) * (math
            .sqrt(d.max) - math.sqrt(d.min)) / r.length
          sqrtX * sqrtX

        def withRange(range: Range): Option[Scale] = sqrt(d, range)

        def withDomain(domain: Domain): Option[Scale] = sqrt(domain, r)
      )
