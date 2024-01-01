package ff4s.canvas

enum Easing:
  case CubicIn
  case CubicInOut
  case CubicOut
  case Linear

  def apply(t: Double): Double = this match
    case CubicInOut =>
      if t < 0.5 then 4 * t * t * t else 1 - Math.pow(-2 * t + 2, 3) / 2
    case CubicIn  => t * t * t
    case CubicOut => 1 - Math.pow(1 - t, 3)
    case Linear   => t
