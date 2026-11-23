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

// expected values computed with d3-array's `ticks`
class TicksSuite extends munit.FunSuite:

  test("unit interval"):
    assertEquals(
      Ticks.ticks(0, 1, 10),
      List(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
    )
    assertEquals(Ticks.ticks(0, 1, 5), List(0.0, 0.2, 0.4, 0.6, 0.8, 1.0))
    assertEquals(Ticks.ticks(0, 1, 2), List(0.0, 0.5, 1.0))
    assertEquals(Ticks.ticks(0, 1, 1), List(0.0, 1.0))

  test("symmetric interval around zero"):
    assertEquals(Ticks.ticks(-10, 10, 5), List(-10.0, -5.0, 0.0, 5.0, 10.0))

  test("ticks stay inside the interval"):
    assertEquals(Ticks.ticks(1, 9, 3), List(2.0, 4.0, 6.0, 8.0))
    assertEquals(Ticks.ticks(0.13, 0.87, 4), List(0.2, 0.4, 0.6, 0.8))

  test("large values"):
    assertEquals(
      Ticks.ticks(0, 1000, 4),
      List(0.0, 200.0, 400.0, 600.0, 800.0, 1000.0)
    )

  test("count must be positive"):
    intercept[IllegalArgumentException](Ticks.ticks(0, 1, 0))
