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

// License pertaining to d3-array
//
// Copyright 2010-2023 Mike Bostock
//
// Permission to use, copy, modify, and/or distribute this software for any purpose
// with or without fee is hereby granted, provided that the above copyright notice
// and this permission notice appear in all copies.
//
// THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
// REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
// FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
// INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
// OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
// TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
// THIS SOFTWARE.

package ff4s.canvas

// taken from d3-array
object Ticks:

  private val e10 = math.sqrt(50)
  private val e5 = math.sqrt(10)
  private val e2 = math.sqrt(2)

  def tickSpec(start: Double, stop: Double, count: Int): (Long, Long, Double) =
    require(count > 0)
    val step = (stop - start) / count
    val power = math.floor(math.log10(step))
    val error = step / math.pow(10.0, power)
    val factor =
      if (error >= e10) then 10.0
      else if error >= e5 then 5.0
      else if error >= e2 then 2.0
      else 1.0
    val inc = math.pow(10.0, power) * factor
    var i1 = math.round(start / inc)
    var i2 = math.round(stop / inc)
    if (i1 * inc < start) {
      i1 += 1
    }
    if (i2 * inc > stop) {
      i2 -= 1
    }
    (i1, i2, inc)

  def ticks(start: Double, stop: Double, count: Int): List[Double] =
    val (i1, i2, inc) = tickSpec(start, stop, count)
    (i1 to i2)
      .map(_ * inc)
      .map(tick => math.round(tick * 10000.0) / 10000.0)
      .toList
