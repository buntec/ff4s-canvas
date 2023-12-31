package ff4s.canvas

object syntax:

  extension [A: Drawable](a: A)
    def draw(at: Point): Draw[Unit] =
      Drawable[A].draw(a, at)
