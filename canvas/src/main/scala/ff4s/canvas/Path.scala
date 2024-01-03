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

import cats.free.Free
import cats.free.Free.*
import org.scalajs.dom.Path2D
import cats.Id
import cats.~>

sealed trait PathA[A]

object PathA:
  case class ClosePath() extends PathA[Unit]
  case class MoveTo(x: Double, y: Double) extends PathA[Unit]
  case class LineTo(x: Double, y: Double) extends PathA[Unit]
  case class BezierCurveTo(
      cp1x: Double,
      cp1y: Double,
      cp2x: Double,
      cp2y: Double,
      x: Double,
      y: Double
  ) extends PathA[Unit]

  case class QuadraticCurveTo(cpx: Double, cpy: Double, x: Double, y: Double)
      extends PathA[Unit]

  case class Arc(
      x: Double,
      y: Double,
      radius: Double,
      startAngle: Double,
      endAngle: Double
  ) extends PathA[Unit]

  case class ArcTo(
      x1: Double,
      y1: Double,
      x2: Double,
      y2: Double,
      radius: Double
  ) extends PathA[Unit]

  case class Ellipse(
      x: Double,
      y: Double,
      radiusX: Double,
      radiusY: Double,
      rotation: Double,
      startAngle: Double,
      endAngle: Double
  ) extends PathA[Unit]

  case class Rect(x: Double, y: Double, width: Double, height: Double)
      extends PathA[Unit]

type Path[A] = Free[PathA, A]

object Path:

  import PathA.*

  private def compiler(p2d: Path2D): PathA ~> Id = new ~>[PathA, Id]:
    def apply[A](pa: PathA[A]): Id[A] = {
      pa match
        case ClosePath()  => p2d.closePath()
        case MoveTo(x, y) => p2d.moveTo(x, y)
        case LineTo(x, y) => p2d.lineTo(x, y)
        case BezierCurveTo(cp1x, cp1y, cp2x, cp2y, x, y) =>
          p2d.bezierCurveTo(cp1x, cp1y, cp2x, cp2y, x, y)
        case QuadraticCurveTo(cpx, cpy, x, y) =>
          p2d.quadraticCurveTo(cpx, cpy, x, y)
        case Arc(x, y, radius, startAngle, endAngle) =>
          p2d.arc(x, y, radius, startAngle, endAngle)
        case ArcTo(x1, y1, x2, y2, radius) =>
          p2d.arcTo(x1, y1, x2, y2, radius)
        case Ellipse(x, y, radiusX, radiusY, rotation, startAngle, endAngle) =>
          p2d.ellipse(x, y, radiusX, radiusY, rotation, startAngle, endAngle)
        case Rect(x, y, width, height) =>
          p2d.rect(x, y, width, height)
    }

  private[canvas] def toPath2D[A](path: Path[A]): Path2D =
    val p2d = new Path2D
    path.foldMap(compiler(p2d))
    p2d

  val closePath: Path[Unit] = liftF[PathA, Unit](ClosePath())

  def moveTo(x: Double, y: Double): Path[Unit] =
    liftF[PathA, Unit](MoveTo(x, y))

  def lineTo(x: Double, y: Double): Path[Unit] =
    liftF[PathA, Unit](LineTo(x, y))

  def bezierCurveTo(
      cp1x: Double,
      cp1y: Double,
      cp2x: Double,
      cp2y: Double,
      x: Double,
      y: Double
  ): Path[Unit] =
    liftF[PathA, Unit](BezierCurveTo(cp1x, cp1y, cp2x, cp2y, x, y))

  def quadraticCurveTo(
      cpx: Double,
      cpy: Double,
      x: Double,
      y: Double
  ): Path[Unit] =
    liftF[PathA, Unit](
      QuadraticCurveTo(
        cpx: Double,
        cpy: Double,
        x: Double,
        y: Double
      )
    )

  def arcTo(
      x1: Double,
      y1: Double,
      x2: Double,
      y2: Double,
      radius: Double
  ): Path[Unit] =
    liftF[PathA, Unit](
      ArcTo(
        x1: Double,
        y1: Double,
        x2: Double,
        y2: Double,
        radius: Double
      )
    )

  def arc(
      x: Double,
      y: Double,
      radius: Double,
      startAngle: Double,
      endAngle: Double
  ): Path[Unit] =
    liftF[PathA, Unit](
      Arc(x, y, radius, startAngle, endAngle)
    )

  def ellipse(
      x: Double,
      y: Double,
      radiusX: Double,
      radiusY: Double,
      rotation: Double,
      startAngle: Double,
      endAngle: Double
  ): Path[Unit] =
    liftF[PathA, Unit](
      Ellipse(
        x: Double,
        y: Double,
        radiusX: Double,
        radiusY: Double,
        rotation: Double,
        startAngle: Double,
        endAngle: Double
      )
    )

  def rect(x: Double, y: Double, width: Double, height: Double): Path[Unit] =
    liftF[PathA, Unit](
      Rect(
        x: Double,
        y: Double,
        width: Double,
        height: Double
      )
    )
