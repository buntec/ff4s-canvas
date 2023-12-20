package ff4s.canvas

import org.scalajs.dom

trait Drawable[A]:
  def draw(a: A, at: Point)(using dom.CanvasRenderingContext2D): Unit

object Drawable:

  def apply[A](using ev: Drawable[A]): Drawable[A] = ev

  extension [A: Drawable](a: A)
    def draw(at: Point)(using dom.CanvasRenderingContext2D): Unit =
      Drawable[A].draw(a, at)
