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

class ScaleSuite extends munit.FunSuite:

  private val eps = 1e-9

  test("linear scale maps domain to range and back"):
    val s = Scale.linear(Domain(0, 10), Range(100, 200)).get
    assertEqualsDouble(s(0), 100, eps)
    assertEqualsDouble(s(10), 200, eps)
    assertEqualsDouble(s(2.5), 125, eps)
    assertEqualsDouble(s(-10), 0, eps) // extrapolates
    assertEqualsDouble(s.inverse(125), 2.5, eps)

  test("linear scale supports an inverted range (canvas y-axis)"):
    val s = Scale.linear(Domain(0, 1), Range(500, 0)).get
    assertEqualsDouble(s(0), 500, eps)
    assertEqualsDouble(s(1), 0, eps)
    assertEqualsDouble(s(0.2), 400, eps)
    assertEqualsDouble(s.inverse(400), 0.2, eps)

  test("degenerate domain or range yields no scale"):
    assertEquals(Scale.linear(Domain(1, 1), Range(0, 1)), None)
    assertEquals(Scale.linear(Domain(0, 1), Range(3, 3)), None)
    assertEquals(Scale.sqrt(Domain(1, 1), Range(0, 1)), None)
    assertEquals(Scale.sqrt(Domain(0, 1), Range(3, 3)), None)

  test("withDomain and withRange keep the other side"):
    val s = Scale.linear(Domain(0, 10), Range(0, 100)).get
    val s2 = s.withDomain(Domain(0, 20)).get
    assertEquals(s2.range, Range(0, 100))
    assertEqualsDouble(s2(10), 50, eps)
    val s3 = s.withRange(Range(0, 50)).get
    assertEquals(s3.domain, Domain(0, 10))
    assertEqualsDouble(s3(10), 50, eps)
    assertEquals(s.withDomain(Domain(5, 5)), None)

  test("sqrt scale maps endpoints and is the inverse of its inverse"):
    val s = Scale.sqrt(Domain(0, 100), Range(0, 10)).get
    assertEqualsDouble(s(0), 0, eps)
    assertEqualsDouble(s(25), 5, eps)
    assertEqualsDouble(s(100), 10, eps)
    List(0.0, 1.0, 7.5, 49.0, 100.0).foreach: x =>
      assertEqualsDouble(s.inverse(s(x)), x, 1e-6)
    val s2 = s.withRange(Range(10, 0)).get
    assertEqualsDouble(s2(25), 5, eps)
    assertEqualsDouble(s2(100), 0, eps)
