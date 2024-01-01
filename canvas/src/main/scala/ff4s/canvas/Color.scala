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

enum Color:
  // format: #DEB887
  case Hex(hex: String)

  // in [0, 255] x [0, 255] x [0, 255]
  case Rgb(r: Int, g: Int, b: Int)

  // in [0.0, 360.0] x [0.0, 100.0] x [0.0, 100.0]
  case Hsl(h: Double, s: Double, l: Double)

  // standard named colors:
  // https://developer.mozilla.org/en-US/docs/Web/CSS/named-color
  case Black
  case Silver
  case Gray
  case White
  case Maroon
  case Red
  case Purple
  case Fuchsia
  case Green
  case Lime
  case Olive
  case Yellow
  case Navy
  case Blue
  case Teal
  case Aqua

  // more named colors
  case AliceBlue
  case AntiqueWhite
  case Aquamarine
  case Azure
  case BlanchedAlmond
  case Burlywood
  // TODO: add missing named colors

  def toHex: Hex = this match
    case hex @ Hex(_)       => hex
    case rgb @ Rgb(_, _, _) => Color.rgbToHex(rgb)
    case hsl @ Hsl(_, _, _) => hsl.toRgb.toHex
    case Black              => Hex("#000000")
    case Silver             => Hex("#c0c0c0")
    case Gray               => Hex("#808080")
    case White              => Hex("#ffffff")
    case Maroon             => Hex("#800000")
    case Red                => Hex("#ff0000")
    case Purple             => Hex("#800080")
    case Fuchsia            => Hex("#ff00ff")
    case Green              => Hex("#008000")
    case Lime               => Hex("#00ff00")
    case Olive              => Hex("#808000")
    case Yellow             => Hex("#ffff00")
    case Navy               => Hex("#000080")
    case Blue               => Hex("#0000ff")
    case Teal               => Hex("#008080")
    case Aqua               => Hex("#00ffff")
    case AliceBlue          => Hex("#f0f8ff")
    case AntiqueWhite       => Hex("#faebd7")
    case Aquamarine         => Hex("#7fffd4")
    case Azure              => Hex("#f0ffff")
    case BlanchedAlmond     => Hex("#ffebcd")
    case Burlywood          => Hex("#deb887")

  def toRgb: Rgb = this match
    case Hex(hex)           => Color.hexToRgb(hex)
    case rgb @ Rgb(_, _, _) => rgb
    case Hsl(h, s, l)       => Color.hslToRgb(h / 360, s / 100, l / 100)
    case other              => other.toHex.toRgb

  def toHsl: Hsl = this match
    case Hex(hex)           => Color.hexToRgb(hex).toHsl
    case rgb @ Rgb(_, _, _) => Color.rgbToHsl(rgb)
    case hsl @ Hsl(_, _, _) => hsl
    case other              => other.toRgb.toHsl

  override def toString: String = toHex.hex

object Color:

  def lerp(c1: Color, c2: Color, t: Double): Color =
    if t <= 0 then c1
    else if t >= 1 then c2
    else lerpHsl(c1.toHsl, c2.toHsl, t)

  given Transition[Color] = new Transition[Color]:
    def apply(a1: Color, a2: Color, t: Double): Color = lerp(a1, a2, t)

  private def rgbToHex(rgb: Rgb): Hex = Hex(
    f"#${rgb.r}%02X${rgb.g}%02X${rgb.b}%02X"
  )

  private def hexToRgb(hex: String): Rgb =
    val n = Integer.valueOf(hex.drop(1), 16)
    val r = (n & 0xff0000) >> 16;
    val g = (n & 0xff00) >> 8;
    val b = (n & 0xff);
    Rgb(r, g, b)

  // assumes h, s, l are in [0, 1]
  private def hslToRgb(h: Double, s: Double, l: Double): Rgb =
    if s == 0 then Rgb((255 * l).toInt, (255 * l).toInt, (255 * l).toInt)
    else
      def hue2rgb(p: Double, q: Double, t0: Double): Double =
        val t = if t0 < 0 then t0 + 1 else if t0 > 1 then t0 - 1 else t0
        if t < 1.0 / 6 then p + (q - p) * 6 * t
        else if t < 1.0 / 2 then q
        else if t < 2.0 / 3 then p + (q - p) * (2.0 / 3 - t) * 6
        else p

      val q = if l < 0.5 then l * (1 + s) else l + s - l * s
      val p = 2 * l - q

      val r = hue2rgb(p, q, h + 1.0 / 3)
      val g = hue2rgb(p, q, h)
      val b = hue2rgb(p, q, h - 1.0 / 3)
      Rgb(
        math.round(255 * r).toInt,
        math.round(255 * g).toInt,
        math.round(255 * b).toInt
      )

  private def rgbToHsl(rgb: Rgb): Hsl =
    val r = rgb.r / 255.0
    val g = rgb.g / 255.0
    val b = rgb.b / 255.0
    val max = math.max(math.max(r, g), b)
    val min = math.min(math.min(r, g), b)
    val l = (max + min) / 2
    if max == min then Hsl(0, 0, 100 * l)
    else
      val d = max - min
      val s = if l > 0.5 then d / (2 - max - min) else d / (max + min)
      val h0 = if max == r then
        val h1 = (g - b) / d
        if g < b then h1 + 6 else h1
      else if max == g then (b - r) / d + 2
      else (r - g) / d + 4 // max == b
      val h = h0 / 6
      Hsl(360 * h, 100 * s, 100 * l)

  private def lerpHsl(c1: Hsl, c2: Hsl, t: Double): Hsl =
    val x1 = math.cos(c1.h / 180 * math.Pi) * c1.s
    val y1 = math.sin(c1.h / 180 * math.Pi) * c1.s
    val z1 = c1.l

    val x2 = math.cos(c2.h / 180 * math.Pi) * c2.s
    val y2 = math.sin(c2.h / 180 * math.Pi) * c2.s
    val z2 = c2.l

    val x = (1 - t) * x1 + t * x2
    val y = (1 - t) * y1 + t * y2
    val z = (1 - t) * z1 + t * z2

    val h0 = math.atan2(y, x) * 180 / math.Pi
    val h = if h0 < 0 then h0 + 360 else h0
    val s = math.sqrt(x * x + y * y)
    val l = z
    Hsl(h, s, l)
