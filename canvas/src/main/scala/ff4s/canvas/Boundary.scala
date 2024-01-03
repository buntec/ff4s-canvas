package ff4s.canvas

trait Boundary[A]:
  def path(a: A, origin: Point): Path[Unit]

object Boundary:

  def apply[A](using ev: Boundary[A]): Boundary[A] = ev

trait BoundarySyntax:

  extension [A: Boundary](a: A)
    def boundingPath(origin: Point): Path[Unit] = Boundary[A].path(a, origin)
