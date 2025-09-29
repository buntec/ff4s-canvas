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

import math.pow

final case class Point(x: Double, y: Double)

object Point:

  given Transition[Point] = Transition.transition((p1, p2, t) =>
    Point(t * p2.x + (1 - t) * p1.x, t * p2.y + (1 - t) * p1.y)
  )

  extension (p: Point)
    def distance2To(other: Point): Double =
      pow(p.x - other.x, 2) + pow(p.y - other.y, 2)
