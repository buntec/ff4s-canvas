package ff4s.canvas

enum FillRule:
  case Nonzero, EvenOdd

enum TextAlign:
  case Start, End, Left, Right, Center

enum TextBaseline:
  case Top, Middle, Bottom, Hanging, Alphabetic, Ideographic

enum Direction:
  case Up, Down

opaque type DOMHighResTimeStamp = Double

object DOMHighResTimeStamp:

  private[canvas] def apply(t: Double): DOMHighResTimeStamp = t

  extension (t: DOMHighResTimeStamp) def toMillis: Double = t
