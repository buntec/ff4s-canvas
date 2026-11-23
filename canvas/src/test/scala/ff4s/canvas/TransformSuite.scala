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

import Scale.Domain
import Scale.Range

class TransformSuite extends munit.FunSuite:

  private val eps = 1e-9

  private def assertPoint(obtained: Point, expected: Point)(using
      munit.Location
  ): Unit =
    assertEqualsDouble(obtained.x, expected.x, eps)
    assertEqualsDouble(obtained.y, expected.y, eps)

  private val t1 = Transform(10, -5, 2)
  private val t2 = Transform(-3, 7, 0.5)
  private val p = Point(4, 6)

  test("apply scales then translates"):
    assertPoint(t1(p), Point(18, 7))
    assertPoint(Transform.identity(p), p)
    assertPoint(Transform.translate(1, 2)(p), Point(5, 8))
    assertPoint(Transform.scale(3)(p), Point(12, 18))

  test("invert undoes apply"):
    assertPoint(t1.invert(t1(p)), p)
    assertPoint(t2.invert(t2(p)), p)

  test("after and andThen compose in opposite orders"):
    assertPoint(t1.after(t2)(p), t1(t2(p)))
    assertPoint(t1.andThen(t2)(p), t2(t1(p)))

  test("inverse composes to the identity"):
    assertPoint(t1.inverse.after(t1)(p), p)
    assertPoint(t1.after(t1.inverse)(p), p)

  test("rescaleX zooms the domain of a scale"):
    val s = Scale.linear(Domain(0, 100), Range(0, 500)).get
    val zoomed = Transform.scale(2).rescaleX(s).get
    assertEquals(zoomed.range, s.range)
    assertEqualsDouble(zoomed.domain.x1, 0, eps)
    assertEqualsDouble(zoomed.domain.x2, 50, eps)
    val panned = Transform.translate(100, 0).rescaleX(s).get
    assertEqualsDouble(panned.domain.x1, -20, eps)
    assertEqualsDouble(panned.domain.x2, 80, eps)

  test("rescaled scale maps a data point to where the transform puts it"):
    val s = Scale.linear(Domain(-1, 1), Range(400, 0)).get
    val t = Transform(30, 50, 1.5)
    val rescaled = t.rescaleY(s).get
    List(-1.0, -0.3, 0.0, 0.8).foreach: y =>
      assertEqualsDouble(rescaled(y), t.applyY(s(y)), 1e-6)
