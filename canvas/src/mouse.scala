package ff4s.canvas
package mouse

import org.scalajs.dom

def getPos(ev: dom.MouseEvent)(using ctx: dom.CanvasRenderingContext2D): Point =
  val bb = ctx.canvas.getBoundingClientRect()
  val x = (ev.clientX - bb.left) * ctx.canvas.width / bb.width
  val y = (ev.clientY - bb.top) * ctx.canvas.height / bb.height
  Point(x, y)
