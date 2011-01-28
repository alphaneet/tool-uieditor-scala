package neet.tool.editor.ui 
import editor._

import swing.Swing._
import swing._
import swing.event._

object Parts extends ComponentsCollection {
  me =>

  lazy val ui = new BoxPanel(Orientation.Vertical) { 
  new ComponentsFactory(me) {
    List(
      List(label("-parts-")),
      List(button("label"), button("button"), button("textfield")),
      List(button("point"), button("rect"))
    ) foreach { c =>
      contents += new FlowPanel { contents ++= c }
    }
  }}

  def create(kind:String):Element = {
    val p = Parameter
    val e = new Element(kind) {
      name = p.text("name"); text = p.text("text")
      _x = p.toInt("x"); _y = p.toInt("y")
      _w = p.toInt("w"); _h = p.toInt("h")
      _size = p.toInt("size")
    }

    Layer += e
    e
  }

  def labelClicked      { create("label")     }
  def buttonClicked     { create("button")    }
  def textfieldClicked  { create("textfield") }
  def pointClicked      { create("point")     }
  def rectClicked       { create("rect")      }
}
