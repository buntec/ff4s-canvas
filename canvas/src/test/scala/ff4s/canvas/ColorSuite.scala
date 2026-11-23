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

import cats.syntax.all.*

class ColorSuite extends munit.FunSuite:

  import Color.*

  test("named colors map to their CSS hex values"):
    assertEquals(Black.toHex, Hex("#000000"))
    assertEquals(White.toHex, Hex("#ffffff"))
    assertEquals(Burlywood.toHex, Hex("#deb887"))
    assertEquals(Burlywood.toString, "#deb887")

  test("hex to rgb"):
    assertEquals(Hex("#deb887").toRgb, Rgb(222, 184, 135))
    assertEquals(Hex("#DEB887").toRgb, Rgb(222, 184, 135))
    assertEquals(Navy.toRgb, Rgb(0, 0, 128))

  test("rgb to hex is zero-padded"):
    assertEquals(Rgb(1, 2, 3).toHex, Hex("#010203"))
    assertEquals(Rgb(222, 184, 135).toHex, Hex("#DEB887"))

  test("rgb to hsl for primary colors"):
    assertEquals(Red.toHsl, Hsl(0, 100, 50))
    assertEquals(Lime.toHsl, Hsl(120, 100, 50))
    assertEquals(Blue.toHsl, Hsl(240, 100, 50))
    assertEquals(White.toHsl, Hsl(0, 0, 100))
    assertEquals(Black.toHsl, Hsl(0, 0, 0))

  test("hsl to rgb for primary colors"):
    assertEquals(Hsl(0, 100, 50).toRgb, Rgb(255, 0, 0))
    assertEquals(Hsl(120, 100, 50).toRgb, Rgb(0, 255, 0))
    assertEquals(Hsl(240, 100, 50).toRgb, Rgb(0, 0, 255))

  test("rgb -> hsl -> rgb round-trips"):
    val colors = Gen.listOfN(500, Color.gen).run(Gen.init)(1)
    colors.foreach: c =>
      assertEquals(c.toHsl.toRgb, c.toRgb, s"color $c")

  test("greys round-trip through hsl"):
    (0 to 255).foreach: v =>
      val grey = Rgb(v, v, v)
      assertEquals(grey.toHsl.toRgb, grey)

  test("lighten and darken adjust lightness and clamp to [0, 100]"):
    assertEquals(Hsl(0, 100, 50).lighten(10), Hsl(0, 100, 60))
    assertEquals(Hsl(0, 100, 50).darken(10), Hsl(0, 100, 40))
    assertEquals(Hsl(0, 100, 95).lighten(10), Hsl(0, 100, 100))
    assertEquals(Hsl(0, 100, 5).darken(10), Hsl(0, 100, 0))

  test("lerp returns the endpoints outside of (0, 1)"):
    assertEquals(lerp(Red, Blue, 0), Red)
    assertEquals(lerp(Red, Blue, -1), Red)
    assertEquals(lerp(Red, Blue, 1), Blue)
    assertEquals(lerp(Red, Blue, 2), Blue)

  test("lerp interpolates lightness between black and white"):
    val Hsl(_, s, l) = lerp(Black, White, 0.25).toHsl
    assertEqualsDouble(s, 0, 1e-9)
    assertEqualsDouble(l, 25, 1e-9)

  test("Show instance uses the hex representation"):
    assertEquals((Red: Color).show, "#ff0000")
