package ff4s.canvas

import cats.syntax.all.*

trait Transition[A]:

  // for t<=0 should return a1
  // for t>=1 should return a2
  // for t in (0, 1) should "interpolate" between a1 and a2
  def apply(a1: A, a2: A, t: Double): A

object Transition:

  def apply[A](using ev: Transition[A]): Transition[A] = ev

  /** f will be called with t in (0, 1) */
  def transition[A](f: (A, A, Double) => A): Transition[A] = new Transition[A]:
    def apply(a1: A, a2: A, t: Double): A =
      if t <= 0 then a1
      else if t >= 1 then a2
      else f(a1, a2, t)

  def none[A]: Transition[A] = new Transition[A]:
    def apply(a1: A, a2: A, t: Double): A =
      if t <= 0 then a1
      else a2

  given [A: Transition]: Transition[Option[A]] =
    new Transition[Option[A]]:
      def apply(a1: Option[A], a2: Option[A], t: Double): Option[A] =
        if t <= 0 then a1
        else if t >= 1 then a2
        else
          (a1, a2) match
            case (Some(a1), Some(a2)) => Transition[A](a1, a2, t).some
            case _                    => a2

  given [A: Transition]: Transition[Seq[A]] =
    new Transition[Seq[A]]:
      def apply(a1: Seq[A], a2: Seq[A], t: Double): Seq[A] =
        if t <= 0 then a1
        else if t >= 1 then a2
        else
          a1.map(_.some)
            .zipAll(a2.map(_.some), None, None)
            .map((a1, a2) => Transition[Option[A]](a1, a2, t))
            .collect:
              case Some(a) => a

  given [A: Transition]: Transition[List[A]] =
    new Transition[List[A]]:
      def apply(a1: List[A], a2: List[A], t: Double): List[A] =
        if t <= 0 then a1
        else if t >= 1 then a2
        else
          a1.map(_.some)
            .zipAll(a2.map(_.some), None, None)
            .map((a1, a2) => Transition[Option[A]](a1, a2, t))
            .collect:
              case Some(a) => a

  given [K, V: Transition]: Transition[Map[K, V]] =
    new Transition[Map[K, V]]:
      def apply(a1: Map[K, V], a2: Map[K, V], t: Double): Map[K, V] =
        if t <= 0 then a1
        else if t >= 1 then a2
        else
          a2.map((k, v2) =>
            k -> a1.get(k).fold(v2)(v1 => Transition[V](v1, v2, t))
          )

trait TransitionSyntax:

  extension [A: Transition](a: A)
    def transitionTo(a2: A)(t: Double): A = Transition[A](a, a2, t)
