package neet.tool.editor.ui 
import swing.Swing._
import swing.{Label, Button, TextField}
import swing.event._

import java.awt.Dimension

/**
 * @see ComponentsFactory
 */
trait ComponentsCollection { 
  val buttons = collection.mutable.Map[String, Button]()
  val textfields = collection.mutable.Map[String, TextField]()

  // TODO:updateと同じような記述をdefできないか調べる
  // text("aiueo") = hoge 見たいな感じをdefでやりたい
  object text { 
    def apply(name:String):String = textfields(name).text
    def update(name:String, text:Any) {
      textfields(name).text = text.toString
    }
  }

  def toInt(name:String):Option[Int] = {
    try { 
      Some(text(name).toInt)
    } catch {
      case _:NumberFormatException =>
        text(name) = ""
        None
    }
  }
}


/**
 * コンポネーションを作るときのショートカットメソッドを用意したクラス
 * 基本的にswingのコンポネーション名の小文字のcreateメソッドを用意してる
 * 
 * デフォルトのコンポネーションのパラメーターを変更したい場合は
 * object コンポネーション名.要素で変更
 * 
 * 個別に変更したい場合はメソッド名_でinitメソッドを渡せる
 *
 * @see ComponentsCollection
 */
class ComponentsFactory(target:AnyRef, data:ComponentsCollection) { 
  def this(target:ComponentsCollection) = this(target, target)

  /**
   * default label parameter
   */
  object label {
    var font = Main.font
  }

  def label(_name:String):Label = {
    new Label(_name) {
      font = label.font
    }
  }
  def label_(_name:String)(init:Label => Unit):Label = {
    val instance = label(_name)
    init(instance)
    instance
  }

  /**
   * default button parameter
   */
  object button {
    var font = Main.font
    var preferredSize:Dimension = (50, 18)
    var margin = new java.awt.Insets(2, 2, 2, 2)
  }

  def button(_name:String, _text:String=null):Button = {
    new Button(if(_text==null) _name else _text) {
      data.buttons += _name -> this

      font = button.font
      preferredSize = button.preferredSize
      margin = button.margin

      val method = new Method(target, _name+"Clicked")
      reactions += { case _:ButtonClicked => method.invoke() }
    }
  }
  def button_(_name:String, _text:String=null)
    (init:Button => Unit):Button = {

    val instance = button(_name, _text)
    init(instance)
    instance
  }

  /**
   * default textfield parameter
   */
  object textfield {
    var font = Main.font
    var preferredSize:Dimension = (50, 18)
    var keyPressed = (c:TextField, e:KeyPressed) => {}
  }

  def textfield(_name:String):TextField = {
    new TextField {
      data.textfields += _name -> this

      font = textfield.font
      preferredSize = textfield.preferredSize

      listenTo(keys)
      reactions += {
        case e:KeyPressed => textfield.keyPressed(this, e)
      }
    }
  }
  def textfield_(_name:String)
    (init:TextField => Unit):TextField = {

    val instance = textfield(_name)
    init(instance)
    instance
  }
}
