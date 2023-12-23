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

  case class Domain(x1: Double, x2: Double):
    def length = x2 - x1

  case class Range(x1: Double, x2: Double):
    def length = x2 - x1

  def linear(d: Domain, r: Range): Option[Scale] =
    (d.length != 0 && r.length != 0)
      .guard[Option]
      .as(
        new Scale:
          def apply(x: Double): Double =
            r.x1 + (x - d.x1) / d.length * r.length

          def inverse(y: Double): Double =
            d.x1 + (y - r.x1) / r.length * d.length

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

        def apply(x: Double): Double = r.x1 + r.length * (math.sqrt(x) - math
          .sqrt(d.x1)) / (math.sqrt(d.x2) - math.sqrt(d.x1))

        def inverse(y: Double): Double =
          val sqrtX = math.sqrt(d.x1) + (y - r.x1) * (math.sqrt(d.x2) - math
            .sqrt(d.x1)) / r.length
          sqrtX * sqrtX

        def withRange(range: Range): Option[Scale] = sqrt(d, range)

        def withDomain(domain: Domain): Option[Scale] = sqrt(domain, r)
      )
