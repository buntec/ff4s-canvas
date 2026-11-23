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

import ff4s.canvas.syntax.*

class TransitionSuite extends munit.FunSuite:

  test("Double interpolates linearly and clamps t"):
    assertEqualsDouble(Transition[Double](1, 3, 0.25), 1.5, 1e-12)
    assertEquals(Transition[Double](1, 3, -1), 1.0)
    assertEquals(Transition[Double](1, 3, 2), 3.0)

  test("Int rounds"):
    assertEquals(Transition[Int](0, 10, 0.26), 3)
    assertEquals(Transition[Int](0, 10, 0.24), 2)

  test("String replaces characters from the left"):
    assertEquals(Transition[String]("aaaa", "bbbb", 0.5), "bbaa")
    assertEquals(Transition[String]("aaaa", "bbbb", 1), "bbbb")

  test("Option only interpolates between two Somes"):
    assertEquals(Transition[Option[Double]](Some(0), Some(2), 0.5), Some(1.0))
    assertEquals(Transition[Option[Double]](None, Some(2), 0.5), Some(2.0))
    assertEquals(Transition[Option[Double]](Some(0), None, 0.5), None)

  test("List interpolates pairwise and keeps the new tail"):
    assertEquals(
      Transition[List[Double]](List(0, 0), List(2, 4, 6), 0.5),
      List(1.0, 2.0, 6.0)
    )
    assertEquals(
      Transition[List[Double]](List(0, 0, 0), List(2), 0.5),
      List(1.0)
    )

  test("Map interpolates shared keys and keeps the new key set"):
    assertEquals(
      Transition[Map[String, Double]](
        Map("a" -> 0, "b" -> 10),
        Map("a" -> 2, "c" -> 5),
        0.5
      ),
      Map("a" -> 1.0, "c" -> 5.0)
    )

  test("Point interpolates both coordinates"):
    assertEquals(Point(0, 10).transitionTo(Point(4, 20))(0.25), Point(1, 12.5))

  test("none and delayed"):
    assertEquals(Transition.none[Int](1, 2, 0), 1)
    assertEquals(Transition.none[Int](1, 2, 0.01), 2)
    assertEquals(Transition.delayed[Int](1, 2, 0.99), 1)
    assertEquals(Transition.delayed[Int](1, 2, 1), 2)

  test("Marker keeps the target shape and interpolates size and color"):
    val m1 = Marker.Circle(10, Color.Black, fill = true)
    val m2 = Marker.Square(20, Color.White, fill = false)
    val mid = m1.transitionTo(m2)(0.5)
    mid match
      case Marker.Square(size, color, fill) =>
        assertEqualsDouble(size, 15, 1e-12)
        assertEqualsDouble(color.toHsl.l, 50, 1e-9)
        assertEquals(fill, false)
      case other => fail(s"unexpected marker $other")
