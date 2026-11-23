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

class GenSuite extends munit.FunSuite:

  private def sample[A](g: Gen[A], n: Int = 1000): List[A] =
    Gen.listOfN(n, g).run(Gen.init)(1)

  test("generators are deterministic"):
    val g = Gen.listOfN(10, Gen.int)
    assertEquals(g.run(Gen.init), g.run(Gen.init))

  test("setSeed changes the stream, reset restores it"):
    val g = Gen.int
    val (_, a) = g.run(Gen.init)
    val (_, b) = (Gen.setSeed(12345L) *> g).run(Gen.init)
    val (_, c) = (Gen.setSeed(12345L) *> Gen.reset *> g).run(Gen.init)
    assertNotEquals(a, b)
    assertEquals(a, c)

  test("view yields successive values"):
    val xs = Gen.int.view.take(5).toList
    assertEquals(xs.distinct.size, 5)
    assertEquals(xs, Gen.listOfN(5, Gen.int).run(Gen.init)(1))

  test("between stays within bounds"):
    sample(Gen.between(-3, 4)).foreach(i => assert(i >= -3 && i < 4))
    sample(Gen.between(0.5, 0.75)).foreach(x => assert(x >= 0.5 && x < 0.75))
    assertEquals(sample(Gen.between(-3, 4)).toSet, (-3 until 4).toSet)

  test("choose and weightedChoose pick from the given values"):
    assertEquals(sample(Gen.choose("a", "b", "c")).toSet, Set("a", "b", "c"))
    assertEquals(
      sample(Gen.weightedChoose("a" -> 1, "b" -> 0, "c" -> 3)).toSet,
      Set("a", "c")
    )

  test("alphaNumericString has the right length and alphabet"):
    val s = Gen.alphaNumericString(200).run(Gen.init)(1)
    assertEquals(s.length, 200)
    assert(s.forall(_.isLetterOrDigit), s)

  test("Color.gen produces valid rgb colors"):
    sample(Color.gen).foreach:
      case Color.Rgb(r, g, b) =>
        assert(List(r, g, b).forall(c => c >= 0 && c <= 255))
      case other => fail(s"unexpected $other")
