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

enum Easing:
  case CubicIn
  case CubicInOut
  case CubicOut
  case Linear

  def apply(t: Double): Double = this match
    case CubicInOut =>
      if t < 0.5 then 4 * t * t * t else 1 - Math.pow(-2 * t + 2, 3) / 2
    case CubicIn  => t * t * t
    case CubicOut => 1 - Math.pow(1 - t, 3)
    case Linear   => t
