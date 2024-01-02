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

enum Shape:

  case Circle(radius: Double, stroke: Option[Color], fill: Option[Color])

  case EquilateralTriangle(
      sideLength: Double,
      centered: Boolean,
      direction: Direction,
      stroke: Option[Color],
      fill: Option[Color]
  )

  case Rectangle(
      width: Double,
      height: Double,
      centered: Boolean,
      stroke: Option[Color],
      fill: Option[Color]
  )

  case Cross(sideLength: Double, stroke: Option[Color])

object Shape:

  private val SQRT_3 = math.sqrt(3)

  given Drawable[Shape] = new Drawable[Shape]:
    def draw(shape: Shape, at: Point): Draw[Unit] =
      import dsl.*
      shape match
        case Circle(radius, stroke, fill) =>
          for
            _ <- save
            _ <- beginPath
            _ <- arc(at.x, at.y, radius, 0, 2 * math.Pi, false)
            _ <- stroke.foldMapM(color => strokeStyle(color) *> dsl.stroke)
            _ <- fill.foldMapM(color => fillStyle(color) *> dsl.fill)
            _ <- restore
          yield ()

        case Cross(sideLength, stroke) =>
          val h = sideLength / 2
          for
            _ <- save
            _ <- beginPath
            _ <- moveTo(at.x - h, at.y - h)
            _ <- lineTo(at.x + h, at.y + h)
            _ <- moveTo(at.x + h, at.y - h)
            _ <- lineTo(at.x - h, at.y + h)
            _ <- stroke.foldMapM(color => strokeStyle(color) *> dsl.stroke)
            _ <- restore
          yield ()

        case EquilateralTriangle(
              sideLength,
              centered,
              direction,
              stroke,
              fill
            ) =>
          val l = sideLength
          val h = l * SQRT_3 / 2
          val sign = direction match
            case Direction.Up   => 1
            case Direction.Down => -1

          val x1 = at.x
          val y1 = if centered then at.y - h / 2 else at.y
          val x2 = x1 - l / 2
          val y2 = y1 + sign * h
          val x3 = x1 + l / 2
          val y3 = y1 + sign * h
          for
            _ <- save
            _ <- beginPath
            _ <- moveTo(x1, y1)
            _ <- lineTo(x2, y2)
            _ <- lineTo(x3, y3)
            _ <- lineTo(x1, y1)
            _ <- stroke.foldMapM(color => strokeStyle(color) *> dsl.stroke)
            _ <- fill.foldMapM(color => fillStyle(color) *> dsl.fill)
            _ <- restore
          yield ()

        case Rectangle(width, height, centered, stroke, fill) =>
          val x = if centered then at.x - width / 2 else at.x
          val y = if centered then at.y - height / 2 else at.y
          for
            _ <- save
            _ <- beginPath
            _ <- rect(x, y, width, height)
            _ <- stroke.foldMapM(color => strokeStyle(color) *> dsl.stroke)
            _ <- fill.foldMapM(color => fillStyle(color) *> dsl.fill)
            _ <- restore
          yield ()
