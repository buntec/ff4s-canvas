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

import cats.Show

enum FillRule:
  case Nonzero, EvenOdd

object FillRule:

  given Show[FillRule] = Show.show(_ match
    case Nonzero => "nonzero"
    case EvenOdd => "evenodd"
  )

enum TextAlign:
  case Start, End, Left, Right, Center

enum TextBaseline:
  case Top, Middle, Bottom, Hanging, Alphabetic, Ideographic

enum Direction:
  case Up, Down

opaque type DOMHighResTimeStamp = Double

object DOMHighResTimeStamp:

  private[canvas] def apply(t: Double): DOMHighResTimeStamp = t

  extension (t: DOMHighResTimeStamp) def toMillis: Double = t
