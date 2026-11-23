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

class ShapeSuite extends munit.FunSuite:

  private val eps = 1e-9
  private val at = Point(100, 50)

  /** vertices visited by `moveTo`/`lineTo` in a draw program */
  private def drawnVertices[A: Drawable](a: A): Vector[Point] =
    Recorder
      .draw(a.draw(at))
      .collect:
        case DrawA.MoveTo(x, y) => Point(x, y)
        case DrawA.LineTo(x, y) => Point(x, y)

  /** vertices visited by `moveTo`/`lineTo` in a boundary path */
  private def boundaryVertices[A: Boundary](a: A): Vector[Point] =
    Recorder
      .path(a.boundingPath(at))
      .collect:
        case PathA.MoveTo(x, y) => Point(x, y)
        case PathA.LineTo(x, y) => Point(x, y)

  private def assertPoints(obtained: Seq[Point], expected: Seq[Point])(using
      munit.Location
  ): Unit =
    assertEquals(obtained.size, expected.size)
    obtained
      .zip(expected)
      .foreach: (o, e) =>
        assertEqualsDouble(o.x, e.x, eps)
        assertEqualsDouble(o.y, e.y, eps)

  private def triangle(centered: Boolean, direction: Direction) =
    Shape.EquilateralTriangle(10, centered, direction, Some(Color.Red), None)

  test("drawing a shape is wrapped in save/restore and starts a new path"):
    val ops = Recorder.draw(triangle(true, Direction.Up).draw(at))
    assertEquals(ops.take(2), Vector(DrawA.Save(), DrawA.BeginPath()))
    assertEquals(ops.last, DrawA.Restore())

  test("stroke and fill are only applied when given"):
    val strokeOnly = Recorder.draw(
      (Shape.Circle(5, Some(Color.Red), None): Shape).draw(at)
    )
    assert(strokeOnly.contains(DrawA.SetStrokeStyle(Color.Red)))
    assert(strokeOnly.contains(DrawA.Stroke()))
    assert(!strokeOnly.contains(DrawA.Fill()))
    val fillOnly = Recorder.draw(
      (Shape.Circle(5, None, Some(Color.Blue)): Shape).draw(at)
    )
    assert(fillOnly.contains(DrawA.SetFillStyle(Color.Blue)))
    assert(fillOnly.contains(DrawA.Fill()))
    assert(!fillOnly.contains(DrawA.Stroke()))

  test("circle is drawn and bounded by the same arc"):
    val c: Shape = Shape.Circle(5, Some(Color.Red), None)
    assert(
      Recorder
        .draw(c.draw(at))
        .contains(DrawA.Arc(100, 50, 5, 0, 2 * math.Pi, false))
    )
    assertEquals(
      Recorder.path(c.boundingPath(at)),
      Vector(PathA.Arc(100, 50, 5, 0, 2 * math.Pi))
    )

  test("centered rectangle is centered on the point"):
    val r: Shape = Shape.Rectangle(20, 10, true, Some(Color.Red), None)
    assert(Recorder.draw(r.draw(at)).contains(DrawA.Rect(90, 45, 20, 10)))
    assertEquals(
      Recorder.path(r.boundingPath(at)),
      Vector(PathA.Rect(90, 45, 20, 10))
    )

  test("non-centered rectangle has its corner at the point"):
    val r: Shape = Shape.Rectangle(20, 10, false, None, None)
    assertEquals(
      Recorder.path(r.boundingPath(at)),
      Vector(PathA.Rect(100, 50, 20, 10))
    )

  test("triangle boundary matches the drawn triangle"):
    for
      centered <- List(true, false)
      direction <- List(Direction.Up, Direction.Down)
    do
      val t: Shape = triangle(centered, direction)
      assertPoints(boundaryVertices(t), drawnVertices(t))

  test("centered triangles are vertically centered on the point"):
    val h = 10 * math.sqrt(3) / 2
    List(Direction.Up -> 1, Direction.Down -> -1).foreach: (dir, sign) =>
      val vs = boundaryVertices(triangle(true, dir): Shape)
      val apex = vs.head
      assertEqualsDouble(apex.x, at.x, eps)
      assertEqualsDouble(apex.y, at.y - sign * h / 2, eps)
      vs.tail.foreach(v => assertEqualsDouble(v.y, at.y + sign * h / 2, eps))

  test("cross boundary is the square spanned by its strokes"):
    val c: Shape = Shape.Cross(8, Some(Color.Red))
    assertPoints(
      drawnVertices(c),
      List(Point(96, 46), Point(104, 54), Point(104, 46), Point(96, 54))
    )
    assertEquals(
      Recorder.path(c.boundingPath(at)),
      Vector(PathA.Rect(96, 46, 8, 8))
    )

  test("markers delegate to shapes, size is the diameter/side length"):
    assertEquals(
      Recorder.path(
        (Marker.Circle(10, Color.Red, true): Marker).boundingPath(at)
      ),
      Vector(PathA.Arc(100, 50, 5, 0, 2 * math.Pi))
    )
    assertEquals(
      Recorder.path(
        (Marker.Square(10, Color.Red, true): Marker).boundingPath(at)
      ),
      Vector(PathA.Rect(95, 45, 10, 10))
    )
    val filled =
      Recorder.draw((Marker.Square(10, Color.Red, true): Marker).draw(at))
    assert(filled.contains(DrawA.SetFillStyle(Color.Red)))
    val hollow =
      Recorder.draw((Marker.Square(10, Color.Red, false): Marker).draw(at))
    assert(!hollow.contains(DrawA.Fill()))
    val down: Marker = Marker.Triangle(10, Color.Red, true, isUp = false)
    assertPoints(boundaryVertices(down), drawnVertices(down))
