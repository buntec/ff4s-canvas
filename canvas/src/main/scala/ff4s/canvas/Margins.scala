package ff4s.canvas

case class Margins(
    top: Margin,
    bottom: Margin,
    left: Margin,
    right: Margin
)

object Margins:

  def uniformRelative(p: Double) =
    Margins(
      Margin.Relative(p),
      Margin.Relative(p),
      Margin.Relative(p),
      Margin.Relative(p)
    )

  def uniformAbsolute(px: Int) =
    Margins(
      Margin.Absolute(px),
      Margin.Absolute(px),
      Margin.Absolute(px),
      Margin.Absolute(px)
    )
