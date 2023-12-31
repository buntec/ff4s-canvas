package ff4s.canvas

import cats.syntax.all.*

trait Transition[A]:

  // for t<=0 should return a1
  // for t>=1 should return a2
  // for t in (0, 1) should "interpolate" between a1 and a2
  def apply(a1: A, a2: A, t: Double): A

object Transition:

  def apply[A](using ev: Transition[A]): Transition[A] = ev

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
            .zipAll(a2.map(_.some), none, none)
            .map((a1, a2) => Transition[Option[A]](a1, a2, t))
            .collect:
              case Some(a) => a

trait TransitionSyntax:

  extension [A: Transition](a: A)
    def transitionTo(a2: A)(t: Double): A = Transition[A](a, a2, t)
