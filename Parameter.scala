package neet.tool.editor.ui 
import editor._

import swing.Swing._
import swing._
import swing.event._

object Parameter extends ComponentsCollection { 
  me =>

  lazy val ui = new BoxPanel(Orientation.Vertical) { 
  new ComponentsFactory(me) {
    textfield.keyPressed = (c:TextField, e:KeyPressed) => {
      if(e.key == Key.Enter) updateClicked
    }
    List(
      List(label("-parameter-")),
      List(label("name:"), 
           textfield_("name"){ _.preferredSize = (100, 20) }),
      List(label("text:"), 
           textfield_("text"){ _.preferredSize = (100, 20) }),
      List(label("x:"), textfield("x"), 
           label("y:"), textfield("y")),
      List(label("w:"), textfield("w"), 
           label("h:"), textfield("h")),
      List(label("size:"),textfield("size"),button("update"))
    ) foreach { c =>
      contents += new FlowPanel { contents ++= c }
    }
  }}

  def updateText {
    var name, txt, x, y, w, h, size:Any = ""
    val len = Layer.selected.length

    if(len == 1) {
      val e = Layer.selected.next
      name = e.name; txt = e.text
      x = e.x; y = e.y
      w = e.w; h = e.h
      size = e.size
    } 
    if(len > 1) {
      name = "multi selected"
      // @see main.scala TODO-002
//      val r = Editor.selectedRect
//      x = r.x; y = r.y
//      w = r.width; h = r.height
    }
    text("name") = name; text("text") = txt
    text("x") = x; text("y") = y
    text("w") = w; text("h") = h
    text("size") = size 
  }

  /**
   *  toIntはOption[Int]が返ってくる 
   *  空白や数字でない場合はNoneが返ってくる
   *  各要素はNoneでない場合更新させる 
   *  
   *  各要素のsetterの一部 
   *  <code> 
   *  var _x:Option[Int] = None 
   *  def x_=(v:Option[Int]) { if(v.isDefined) _x = v }
   *  </code>
   *  @see Element
   */
  def updateClicked {
    val len = Layer.selected.length

    if(len == 1) {
      val e = Layer.selected.next
      e.name = text("name"); e.text = text("text")
      e.x = toInt("x"); e.y = toInt("y")
      e.w = toInt("w"); e.h = toInt("h")
      e.size = toInt("size")

      Editor updateComponent e
      Layer updateList e
    } else if(len > 1) {
      val txt = text("text")
      val (x, y) = (toInt("x"), toInt("y"))
      val (w, h) = (toInt("w"), toInt("h"))
      val size = toInt("size")
      Layer.selected foreach { e =>
        if(txt != "") e.text = txt
        e.x = x; e.y = y
        e.w = w; e.h = h
        e.size = size

        Editor updateComponent e
      }
      Layer.updateList
    }
  }
}
