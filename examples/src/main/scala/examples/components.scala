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

package examples

trait Buttons[S, A]:
  dsl: ff4s.Dsl[S, A] =>

  private def btnImpl(
      label0: String,
      onClick0: A,
      cls0: String,
      isDisabled: S => Boolean
  ): V =
    import html.*
    useState: state =>
      button(
        cls := cls0,
        onClick := (_ => Some(onClick0)),
        disabled := isDisabled(state),
        label0
      )

  def btn(
      label: String,
      onClick0: A,
      isDisabled: S => Boolean = _ => false
  ): V = btnImpl(
    label,
    onClick0,
    "px-3 py-2 border rounded bg-gray-800 hover:bg-gray-700 hover:scale-105 active:scale-100",
    isDisabled
  )

  def `btn-sm`(
      label: String,
      onClick0: A,
      isDisabled: S => Boolean = _ => false
  ): V = btnImpl(label, onClick0, "px-2 py-1 border rounded", isDisabled)
