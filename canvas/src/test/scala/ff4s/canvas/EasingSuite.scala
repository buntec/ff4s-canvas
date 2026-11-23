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

class EasingSuite extends munit.FunSuite:

  private val eps = 1e-12

  test("all easings fix the endpoints"):
    Easing.values.foreach: e =>
      assertEqualsDouble(e(0), 0, eps, e.toString)
      assertEqualsDouble(e(1), 1, eps, e.toString)

  test("all easings are monotone on [0, 1]"):
    val ts = (0 to 100).map(_ / 100.0)
    Easing.values.foreach: e =>
      ts.zip(ts.tail)
        .foreach: (a, b) =>
          assert(e(a) <= e(b), s"$e not monotone at $a")

  test("known values"):
    assertEqualsDouble(Easing.Linear(0.3), 0.3, eps)
    assertEqualsDouble(Easing.CubicIn(0.5), 0.125, eps)
    assertEqualsDouble(Easing.CubicOut(0.5), 0.875, eps)
    assertEqualsDouble(Easing.CubicInOut(0.5), 0.5, eps)
    assertEqualsDouble(Easing.CubicInOut(0.25), 0.0625, eps)
    assertEqualsDouble(Easing.CubicInOut(0.75), 0.9375, eps)
