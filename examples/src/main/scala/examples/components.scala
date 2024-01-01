package examples

trait Buttons[F[_], S, A]:

  def btnImpl(
      label0: String,
      onClick0: A,
      cls0: String,
      isDisabled: S => Boolean = _ => false
  )(implicit
      dsl: ff4s.Dsl[F, S, A]
  ): dsl.V =
    import dsl._
    import dsl.html._

    useState: state =>
      button(
        cls := cls0,
        onClick := (_ => Some(onClick0)),
        disabled := isDisabled(state),
        label0
      )

  def btn(label: String, onClick0: A, isDisabled: S => Boolean = _ => false)(
      implicit dsl: ff4s.Dsl[F, S, A]
  ): dsl.V = btnImpl(
    label,
    onClick0,
    "px-3 py-2 border rounded bg-gray-800 hover:bg-gray-700 hover:scale-105 active:scale-100",
    isDisabled
  )

  def `btn-sm`(
      label: String,
      onClick0: A,
      isDisabled: S => Boolean = _ => false
  )(implicit
      dsl: ff4s.Dsl[F, S, A]
  ): dsl.V = btnImpl(label, onClick0, "px-2 py-1 border rounded", isDisabled)
