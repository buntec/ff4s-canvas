/*
 * Copyright 2023 buntec
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ff4s.canvas

import cats.Show
import cats.syntax.all.*

final case class Font(
    style: Option[FontStyle],
    variant: Option[FontVariant],
    weight: Option[FontWeight],
    size: FontSize,
    lineHeight: Option[LineHeight],
    family: FontFamily
):

  def withStyle(style: FontStyle): Font = this.copy(style = style.some)
  def withVariant(variant: FontVariant): Font =
    this.copy(variant = variant.some)
  def withWeight(weight: FontWeight): Font = this.copy(weight = weight.some)
  def withLineHeight(lineHeight: LineHeight): Font =
    this.copy(lineHeight = lineHeight.some)

object Font:

  def apply(size: FontSize, family: FontFamily): Font =
    Font(None, None, None, size, None, family)

  given Show[Font] = Show.show(f =>
    List(
      f.style.map(_.show),
      f.variant.map(_.show),
      f.weight.map(_.show),
      f.size.show.some,
      f.lineHeight.map(_.show),
      f.family.show.some
    ).collect:
      case Some(a) => a
    .mkString(" ")
  )

enum FontStyle:
  case Normal, Italic, Oblique

object FontStyle:
  given Show[FontStyle] = Show.show(_ match
    case Normal  => "normal"
    case Italic  => "italic"
    case Oblique => "oblique"
  )

enum FontVariant:
  case Normal, SmallCaps

object FontVariant:
  given Show[FontVariant] = Show.show(_ match
    case Normal    => "normal"
    case SmallCaps => "small-caps"
  )

enum FontWeight:
  case Number(weight: Int)
  case Thin
  case ExtraLight
  case Light
  case Normal
  case Medium
  case SemiBold
  case Bold
  case ExtraBold
  case Black
  case ExtraBlack
  case Bolder
  case Lighter

object FontWeight:
  given Show[FontWeight] = Show.show(_ match
    case Number(weight) => weight.show
    case Thin           => "100"
    case ExtraLight     => "200"
    case Light          => "300"
    case Normal         => "normal"
    case Medium         => "500"
    case SemiBold       => "600"
    case Bold           => "bold"
    case ExtraBold      => "800"
    case Black          => "900"
    case ExtraBlack     => "950"
    case Bolder         => "bolder"
    case Lighter        => "lighter"
  )

enum FontSize:
  case Medium
  case Px(int: Int)

object FontSize:
  given Show[FontSize] = Show.show(_ match
    case Medium => "medium"
    case Px(n)  => show"${n}px"
  )

enum LineHeight:
  case Normal
  case Unitless(n: Double)

object LineHeight:
  given Show[LineHeight] = Show.show(_ match
    case Normal      => "/normal"
    case Unitless(n) => show"/$n"
  )

enum FontFamily:
  case SystemUI, Serif, SansSerif, Monospace, Cursive,
    UISerif, UISansSerif, UIMonospace

object FontFamily:
  given Show[FontFamily] = Show.show(_ match
    case SystemUI    => "system-ui"
    case Serif       => "serif"
    case SansSerif   => "sans-serif"
    case Monospace   => "monospace"
    case Cursive     => "cursive"
    case UISerif     => "ui-serif"
    case UISansSerif => "ui-sans-serif"
    case UIMonospace => "ui-monospace"
  )
