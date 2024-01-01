package ff4s.canvas

import cats.syntax.all.*

trait Transition[A]:

  /** Should return a1 for t<=0 and a2 for t>=1. For t in (0, 1) it should
    * "interpolate" between a1 and a2. Instances for numeric types should
    * implement linear interpolation; non-linear interpolation can then be
    * achieved by an "easing" function
    */
  def apply(a1: A, a2: A, t: Double): A

object Transition:

  def apply[A](using ev: Transition[A]): Transition[A] = ev

  /** f will be called with t in (0, 1) */
  def transition[A](f: (A, A, Double) => A): Transition[A] = new Transition[A]:
    def apply(a1: A, a2: A, t: Double): A =
      if t <= 0 then a1
      else if t >= 1 then a2
      else f(a1, a2, t)

  /** no transition: jump to the new value immediately */
  def none[A]: Transition[A] = new Transition[A]:
    def apply(a1: A, a2: A, t: Double): A =
      if t <= 0 then a1
      else a2

  /** keep the old value for all t < 1 */
  def delayed[A]: Transition[A] = new Transition[A]:
    def apply(a1: A, a2: A, t: Double): A =
      if t < 1 then a1
      else a2

  given Transition[Double] = transition((a1, a2, t) => (1 - t) * a1 + t * a2)

  given Transition[Float] =
    transition((a1, a2, t) => (1 - t.toFloat) * a1 + t.toFloat * a2)

  given Transition[Int] =
    transition((a1, a2, t) => math.round((1 - t) * a1 + t * a2).toInt)

  given Transition[Long] =
    transition((a1, a2, t) => math.round((1 - t) * a1 + t * a2))

  given Transition[BigDecimal] =
    transition((a1, a2, t) => (1 - t) * a1 + t * a2)

  given Transition[String] =
    transition: (a1, a2, t) =>
      val n = (math.max(a1.length, a2.length) * t).toInt
      a2.take(n) + a1.drop(n)

  given [A: Transition]: Transition[Option[A]] =
    transition((a1, a2, t) =>
      (a1, a2) match
        case (Some(a1), Some(a2)) => Transition[A](a1, a2, t).some
        case _                    => a2
    )

  given [A: Transition]: Transition[Seq[A]] =
    transition((a1, a2, t) =>
      a1.map(_.some)
        .zipAll(a2.map(_.some), None, None)
        .map((a1, a2) => Transition[Option[A]](a1, a2, t))
        .collect:
          case Some(a) => a
    )

  given [A: Transition]: Transition[List[A]] =
    transition((a1, a2, t) =>
      a1.map(_.some)
        .zipAll(a2.map(_.some), None, None)
        .map((a1, a2) => Transition[Option[A]](a1, a2, t))
        .collect:
          case Some(a) => a
    )

  given [K, V: Transition]: Transition[Map[K, V]] =
    transition((a1, a2, t) =>
      a2.map((k, v2) => k -> a1.get(k).fold(v2)(v1 => Transition[V](v1, v2, t)))
    )

trait TransitionSyntax:

  extension [A: Transition](a: A)
    def transitionTo(a2: A)(t: Double): A = Transition[A](a, a2, t)
