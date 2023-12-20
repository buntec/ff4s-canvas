package ff4s.canvas

trait Drawable[A]:
  def draw(a: A, at: Point): Draw[Unit]

object Drawable:

  def apply[A](using ev: Drawable[A]): Drawable[A] = ev

  extension [A: Drawable](a: A)
    def draw(at: Point): Draw[Unit] =
      Drawable[A].draw(a, at)
