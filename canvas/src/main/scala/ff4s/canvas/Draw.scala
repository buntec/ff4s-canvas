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

import cats.Monad
import cats.syntax.all.*
import cats.free.Free
import cats.free.Free.*

sealed trait DrawA[A]

object DrawA:

  case class SetShadowColor(color: Color) extends DrawA[Unit]
  case class SetShadowBlur(blue: Double) extends DrawA[Unit]
  case class SetShadowOffsetX(offset: Double) extends DrawA[Unit]
  case class SetShadowOffsetY(offset: Double) extends DrawA[Unit]

  case class SetStrokeStyle(color: Color) extends DrawA[Unit]
  case class SetLineWidth(width: Double) extends DrawA[Unit]
  case class SetFont(font: Font) extends DrawA[Unit]
  case class SetTextAlign(align: TextAlign) extends DrawA[Unit]
  case class SetTextBaseline(baseline: TextBaseline) extends DrawA[Unit]

  case class Save() extends DrawA[Unit]
  case class Restore() extends DrawA[Unit]
  case class BeginPath() extends DrawA[Unit]
  case class ClosePath() extends DrawA[Unit]
  case class Clip() extends DrawA[Unit]
  case class Fill() extends DrawA[Unit]
  case class Fill2[A](path: Path[A]) extends DrawA[Unit]
  case class SetFillStyle(color: Color) extends DrawA[Unit]
  case class Stroke() extends DrawA[Unit]
  case class ClearRect(x: Double, y: Double, w: Double, h: Double)
      extends DrawA[Unit]

  case class Arc(
      x: Double,
      y: Double,
      radius: Double,
      startAngle: Double,
      endAngle: Double,
      counterclockwise: Boolean
  ) extends DrawA[Unit]

  case class Rect(x: Double, y: Double, width: Double, height: Double)
      extends DrawA[Unit]

  case class FillRect(x: Double, y: Double, width: Double, height: Double)
      extends DrawA[Unit]

  case class MoveTo(x: Double, y: Double) extends DrawA[Unit]

  case class LineTo(x: Double, y: Double) extends DrawA[Unit]

  case class IsPointInPath(x: Double, y: Double, fillRule: FillRule)
      extends DrawA[Boolean]

  case class IsPointInPath2[A](
      path: Path[A],
      x: Double,
      y: Double,
      fillRule: FillRule
  ) extends DrawA[Boolean]

  case class FillText(
      text: String,
      x: Double,
      y: Double,
      maxWidth: Option[Double]
  ) extends DrawA[Unit]
  case class Translate(x: Double, y: Double) extends DrawA[Unit]
  case class Scale(x: Double, y: Double) extends DrawA[Unit]

  // custom
  case class GetMousePos() extends DrawA[Point]
  case class GetTransform() extends DrawA[Transform]
  case class GetMarginTransform() extends DrawA[Transform]
  case class GetWidth() extends DrawA[Int]
  case class GetHeight() extends DrawA[Int]

  // simple KV store
  case class KVPut[T](key: String, value: T) extends DrawA[Unit]
  case class KVGet[T](key: String) extends DrawA[Option[T]]
  case class KVDelete(key: String) extends DrawA[Unit]

type Draw[A] = Free[DrawA, A]

object Draw:
  import DrawA.*

  def pure[A](a: A): Draw[A] = Monad[Draw].pure(a)

  val noop: Draw[Unit] = pure(())

  val save: Draw[Unit] = liftF[DrawA, Unit](Save())

  val restore: Draw[Unit] = liftF[DrawA, Unit](Restore())

  val beginPath: Draw[Unit] = liftF[DrawA, Unit](BeginPath())

  val closePath: Draw[Unit] = liftF[DrawA, Unit](ClosePath())

  val clip: Draw[Unit] = liftF[DrawA, Unit](Clip())

  val fill: Draw[Unit] = liftF[DrawA, Unit](Fill())

  val stroke: Draw[Unit] = liftF[DrawA, Unit](Stroke())

  def fill[A](path: Path[A]): Draw[Unit] = liftF[DrawA, Unit](Fill2(path))

  def withSaveAndRestore[A](da: Draw[A]): Draw[A] =
    save *> da <* restore

  def setShadowBlur(blur: Double): Draw[Unit] =
    liftF[DrawA, Unit](SetShadowBlur(blur))

  def setShadowColor(color: Color): Draw[Unit] =
    liftF[DrawA, Unit](SetShadowColor(color))

  def setShadowOffsetX(offset: Double): Draw[Unit] =
    liftF[DrawA, Unit](SetShadowOffsetX(offset))

  def setShadowOffsetY(offset: Double): Draw[Unit] =
    liftF[DrawA, Unit](SetShadowOffsetY(offset))

  def fillStyle(color: Color): Draw[Unit] =
    liftF[DrawA, Unit](SetFillStyle(color))

  def strokeStyle(color: Color): Draw[Unit] =
    liftF[DrawA, Unit](SetStrokeStyle(color))

  def clearRect(x: Double, y: Double, w: Double, h: Double): Draw[Unit] =
    liftF[DrawA, Unit](ClearRect(x, y, w, h))

  def moveTo(x: Double, y: Double): Draw[Unit] =
    liftF[DrawA, Unit](MoveTo(x, y))

  def lineTo(x: Double, y: Double): Draw[Unit] =
    liftF[DrawA, Unit](LineTo(x, y))

  def isPointInPath(
      x: Double,
      y: Double,
      fillRule: FillRule
  ): Draw[Boolean] =
    liftF[DrawA, Boolean](IsPointInPath(x, y, fillRule))

  def isPointInPath[A](
      path: Path[A],
      x: Double,
      y: Double,
      fillRule: FillRule
  ): Draw[Boolean] =
    liftF[DrawA, Boolean](IsPointInPath2(path, x, y, fillRule))

  def arc(
      x: Double,
      y: Double,
      radius: Double,
      startAngle: Double,
      endAngle: Double,
      counterclockwise: Boolean
  ): Draw[Unit] =
    liftF[DrawA, Unit](
      Arc(x, y, radius, startAngle, endAngle, counterclockwise)
    )

  def rect(x: Double, y: Double, width: Double, height: Double): Draw[Unit] =
    liftF[DrawA, Unit](Rect(x, y, width, height))

  def fillRect(
      x: Double,
      y: Double,
      width: Double,
      height: Double
  ): Draw[Unit] =
    liftF[DrawA, Unit](FillRect(x, y, width, height))

  def lineWidth(width: Double): Draw[Unit] =
    liftF[DrawA, Unit](SetLineWidth(width))

  def font(font: Font): Draw[Unit] =
    liftF[DrawA, Unit](SetFont(font))

  def textAlign(align: TextAlign): Draw[Unit] =
    liftF[DrawA, Unit](SetTextAlign(align))

  def textBaseline(baseline: TextBaseline): Draw[Unit] =
    liftF[DrawA, Unit](SetTextBaseline(baseline))

  def fillText(
      text: String,
      x: Double,
      y: Double,
      maxWidth: Option[Double] = None
  ): Draw[Unit] =
    liftF[DrawA, Unit](FillText(text, x, y, maxWidth))

  def translate(x: Double, y: Double): Draw[Unit] =
    liftF[DrawA, Unit](Translate(x, y))

  def scale(x: Double, y: Double): Draw[Unit] =
    liftF[DrawA, Unit](Scale(x, y))

  // custom

  val mousePos: Draw[Point] = liftF[DrawA, Point](GetMousePos())

  val transform: Draw[Transform] = liftF[DrawA, Transform](GetTransform())

  val marginTransform: Draw[Transform] =
    liftF[DrawA, Transform](GetMarginTransform())

  val width: Draw[Int] =
    liftF[DrawA, Int](GetWidth())

  val height: Draw[Int] =
    liftF[DrawA, Int](GetHeight())

  // kv

  def kvPut[T](key: String, value: T): Draw[Unit] =
    liftF[DrawA, Unit](KVPut[T](key, value))

  def kvGet[T](key: String): Draw[Option[T]] =
    liftF[DrawA, Option[T]](KVGet[T](key))

  def kvDelete(key: String): Draw[Unit] =
    liftF[DrawA, Unit](KVDelete(key))
