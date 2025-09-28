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

enum Marker:
  case Circle(size: Double, color: Color, fill: Boolean)
  case Triangle(
      size: Double,
      color: Color,
      fill: Boolean,
      centered: Boolean = true,
      isUp: Boolean = true
  )
  case Square(size: Double, color: Color, fill: Boolean)
  case Cross(size: Double, color: Color)

  extension (m: Marker)
    def size: Double = m match
      case c: Marker.Circle   => c.size
      case t: Marker.Triangle => t.size
      case s: Marker.Square   => s.size
      case x: Marker.Cross    => x.size

  def withSize(size: Double): Marker = this match
    case Circle(_, color, fill) => Circle(size, color, fill)
    case Triangle(_, color, fill, centered, isUp) =>
      Triangle(size, color, fill, centered, isUp)
    case Square(_, color, fill) => Square(size, color, fill)
    case Cross(_, color)        => Cross(size, color)

  def withColor(color: Color): Marker = this match
    case Circle(size, _, fill) => Circle(size, color, fill)
    case Triangle(size, _, fill, centered, isUp) =>
      Triangle(size, color, fill, centered, isUp)
    case Square(size, _, fill) => Square(size, color, fill)
    case Cross(size, _)        => Cross(size, color)

  def toSize: Double = this match
    case Circle(size, _, _)         => size
    case Triangle(size, _, _, _, _) => size
    case Square(size, _, _)         => size
    case Cross(size, _)             => size

  def toColor: Color = this match
    case Circle(_, color, _)         => color
    case Triangle(_, color, _, _, _) => color
    case Square(_, color, _)         => color
    case Cross(_, color)             => color

object Marker:

  given Drawable[Marker] = new Drawable[Marker]:
    def draw(a: Marker, at: Point): Draw[Unit] =
      a match
        case Circle(size, color, fill) =>
          Drawable[Shape].draw(
            Shape.Circle(
              size / 2,
              color.some,
              fill.guard[Option].as(color)
            ),
            at
          )

        case Triangle(size, color, fill, centered, isUp) =>
          Drawable[Shape].draw(
            Shape.EquilateralTriangle(
              size,
              centered,
              if isUp then Direction.Up else Direction.Down,
              color.some,
              fill.guard[Option].as(color)
            ),
            at
          )

        case Square(size, color, fill) =>
          Drawable[Shape].draw(
            Shape.Rectangle(
              size,
              size,
              true,
              color.some,
              fill.guard[Option].as(color)
            ),
            at
          )

        case Cross(size, color) =>
          Drawable[Shape].draw(
            Shape.Cross(
              size,
              color.some
            ),
            at
          )

  given Boundary[Marker] = new Boundary[Marker]:
    def path(a: Marker, at: Point): Path[Unit] =
      a match
        case Circle(size, color, fill) =>
          Boundary[Shape].path(
            Shape.Circle(
              size / 2,
              None,
              None
            ),
            at
          )

        case Triangle(size, color, fill, centered, isUp) =>
          Boundary[Shape].path(
            Shape.EquilateralTriangle(
              size,
              centered,
              if isUp then Direction.Up else Direction.Down,
              None,
              None
            ),
            at
          )

        case Square(size, color, fill) =>
          Boundary[Shape].path(
            Shape.Rectangle(
              size,
              size,
              true,
              None,
              None
            ),
            at
          )

        case Cross(size, color) =>
          Boundary[Shape].path(Shape.Cross(size, None), at)

  given Transition[Marker] = new Transition[Marker]:
    def apply(a1: Marker, a2: Marker, t: Double): Marker =
      val c1 = a1.toColor
      val c2 = a2.toColor
      val s1 = a1.toSize
      val s2 = a2.toSize
      a2.withColor(Transition[Color](c1, c2, t))
        .withSize(Transition[Double](s1, s2, t))
