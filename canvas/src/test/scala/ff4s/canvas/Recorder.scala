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

import cats.data.State
import cats.~>

/** Pure interpreters for the `Path` and `Draw` algebras that record the
  * instructions instead of executing them against a canvas.
  *
  * Only suitable for programs whose instructions all return `Unit` (which is
  * the case for shapes and markers).
  */
object Recorder:

  private type Rec[A] = State[Vector[Any], A]

  private def record[F[_]]: F ~> Rec = new (F ~> Rec):
    def apply[A](fa: F[A]): Rec[A] =
      State.modify[Vector[Any]](_ :+ fa).map(_.asInstanceOf[A])

  def path[A](p: Path[A]): Vector[PathA[?]] =
    p.foldMap(record[PathA])
      .runS(Vector.empty)
      .value
      .collect:
        case op: PathA[?] => op

  def draw[A](d: Draw[A]): Vector[DrawA[?]] =
    d.foldMap(record[DrawA])
      .runS(Vector.empty)
      .value
      .collect:
        case op: DrawA[?] => op
