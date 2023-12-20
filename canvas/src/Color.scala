package ff4s.canvas

enum Color(override val toString: String):
  case Keyword(name: String) extends Color(name)
  case Hex(hex: String) extends Color(hex)
