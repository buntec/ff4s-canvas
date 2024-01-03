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

trait Boundary[A]:
  def path(a: A, origin: Point): Path[Unit]

object Boundary:

  def apply[A](using ev: Boundary[A]): Boundary[A] = ev

trait BoundarySyntax:

  extension [A: Boundary](a: A)
    def boundingPath(origin: Point): Path[Unit] = Boundary[A].path(a, origin)
