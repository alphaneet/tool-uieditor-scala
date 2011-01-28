package neet.tool.editor.ui 
package editor

import swing.Swing._
import swing._
import swing.event._

import java.awt.{
  Point, Dimension, Rectangle,
  Color, Font, Graphics2D
}

/**
 * scala.swing.Componentの拡張
 *
 * textメソッドが用意されているComponent（Label,Buttonとか）に使用する
 */
private[editor] trait Ex {
  this:Component =>

  // MouseDragged時の移動に使う
  var diff:Point = new Point

  /**
   * swing.scala.Component#size は
   * 警告がでるからオーバーライドしておく
   */
  override def size_=(dim: Dimension) = peer.setSize(dim)
  def location_=(p: Point) = peer.setLocation(p)

  def text:String
  def text_=(t:String)

  def isHit(p:Point):Boolean = 
    isHit(new Rectangle(p.x, p.y, 0, 0))
  def isHit(p:Point, d:Dimension):Boolean = 
    isHit(new Rectangle(p, d))
  def isHit(r:Rectangle):Boolean = {
    val (x, y) = (location.x, location.y)
    val (w, h) = (size.width, size.height)

    (r.x + r.width  > x && r.x < x + w && 
     r.y + r.height > y && r.y < y + h)
  }
}


/**
 * scala.swing.Componentの拡張2
 *
 * textメソッドがないComponentに使用する
 */
private[editor] trait Ex2 extends Ex {
  this:Component =>
  def text:String = ""
  def text_=(t:String) {}
}


/**
 * EditorのComponentなどのデータ管理を担当
 * 
 * @see EditorPanel
 */
object Editor { 
  lazy val frame = new Frame {
    title = "Editor"
    contents = ui
    centerOnScreen
    peer.setDefaultCloseOperation(0)
    resizable = false
  }
  lazy val ui:Panel with SequentialContainer.Wrapper = EditorPanel

  private[editor] val components = 
    collection.mutable.Map[Element, Component with Ex]()

  def wgrid = EditorPanel.wgrid
  def hgrid = EditorPanel.hgrid
  def grid(w:Option[Int], h:Option[Int]) {
    EditorPanel.wgrid = if(w.getOrElse(0) > 0) w else None
    EditorPanel.hgrid = if(h.getOrElse(0) > 0) h else None
    EditorPanel.repaint
  }
  def selectedRect = EditorPanel.selectedRect.rect

  def visible = frame.visible
  def visible_=(v:Boolean) = frame.visible = v

  def location = frame.location
  def location_=(p:Point) = frame.location = p

  def title = frame.title
  def title_=(t:String) = frame.title = t

  def size = ui.preferredSize
  def size_=(dim: Dimension) = {
    // なんども同じサイズのpackをするとサイズ変更できなくなるっぽい
    if(size.width != dim.width || 
      size.height != dim.height) {

      ui.preferredSize = dim
      frame.pack
    }
  }

  def +=(e:Element):this.type = {
    val c:Component with Ex = (e.kind match {
      case "label" => new Label with Ex
      case "button" => new Label with Ex {
        opaque = true
        foreground = Color.white
        background = Color.gray
        border = LineBorder(Color.black)
      }
      case "textfield" => new Label with Ex {
        opaque = true
        horizontalAlignment = Alignment.Left
        background = Color.white
        border = LineBorder(Color.black)
      }
      case "point" => new Component with Ex2 {
        val weight = 5

        // e:Elementの値を書き換えてるので注意
        e.w = weight+1; e.h = weight+1
        size = (e.w, e.h)

        // TODO: 中央に表示させるためにweight/2ドット左上にずらす
        override def paint(g:Graphics2D) {
          g.setColor(Color.red)
          g.fillRect(0, 0, weight, weight)
        }
      }
      case "rect" => new Component with Ex2 {
        override def paint(g:Graphics2D) {
          g.setColor(Color.blue)
          g.drawRect(0, 0, size.width-1, size.height-1)
        }
      }
    })

    components += e -> c

    updateComponent(e)

    ui.contents += c
    ui.repaint
    this
  }

  def -=(e:Element):this.type = {
    ui.contents -= components(e)
    components -= e
    ui.repaint
    this
  }

  def updateComponent(e:Element) {
    val c = components(e)
    c.text = e.textOrElse(e.kind)
    c.location = (e.x, e.y)

    // TODO: こういう場当たり的な対応はよくないので
    // EPointとかクラス化しよう
    if(e.kind == "point") {
      e.w = c.size.width
      e.h = c.size.height
    } else c.size = (e.w, e.h)
    c.font = new Font(Font.SANS_SERIF, Font.PLAIN, e.size)
  }

  def updateSelected = EditorPanel.updateSelected

  def clear {
    ui.contents.clear
    components.clear 
    ui.repaint
  }
}
