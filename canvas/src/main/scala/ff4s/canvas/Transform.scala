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
import org.scalajs.dom.CanvasRenderingContext2D

case class Transform(x: Double, y: Double, k: Double):

  def applyX(x: Double): Double = k * x + this.x
  def applyY(y: Double): Double = k * y + this.y
  def apply(point: Point): Point = Point(applyX(point.x), applyY(point.y))

  def invertX(x: Double): Double = (x - this.x) / k
  def invertY(y: Double): Double = (y - this.y) / k
  def invert(point: Point): Point = Point(invertX(point.x), invertY(point.y))

  def after(t: Transform): Transform =
    Transform(t.x * k + x, t.y * k + y, t.k * k)

  def andThen(t: Transform): Transform = t.after(this)

  def applyToCtx: Draw[Unit] = Draw.translate(x, y) *> Draw.scale(k, k)

  private[canvas] def applyToCtx(ctx: CanvasRenderingContext2D): Unit =
    ctx.translate(x, y)
    ctx.scale(k, k)

  def rescaleX(scale: Scale): Option[Scale] =
    val range = Scale.Range(invertX(scale.range.x1), invertX(scale.range.x2))
    val domain =
      Scale.Domain(scale.inverse(range.x1), scale.inverse(range.x2))
    scale.withDomain(domain)

  def rescaleY(scale: Scale): Option[Scale] =
    val range = Scale.Range(invertY(scale.range.x1), invertY(scale.range.x2))
    val domain =
      Scale.Domain(scale.inverse(range.x1), scale.inverse(range.x2))
    scale.withDomain(domain)

  def inverse: Transform = Transform(-x / k, -y / k, 1 / k)

object Transform:

  def scale(k: Double): Transform = Transform(0, 0, k)

  def translate(x: Double, y: Double): Transform = Transform(x, y, 1)

  def identity: Transform = Transform(0, 0, 1)

  def none: Transform = identity
