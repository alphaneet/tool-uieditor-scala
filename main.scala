package neet.tool.editor.ui 
import editor._

import swing.Swing._
import swing._
import swing.event._

import java.awt.Font

/*
 * TODO LIST
 * scalaTestをつかってかＢＤＤなりＴＤＤでやってみる
 *
 * TODO-001
 * Layerの順番の変更を実装
 * EditorのComponentの描画順位の切り替えがめんどくさいので
 * processingでEditor部分を書き直す
 *
 * TODO-002
 * 複数選択したときのParameter
 * すべての要素のsizeが一緒なら表示する
 * x, y, w, h が表示してくれるのはありがたいが
 * update時に変更がなかったら一括更新しない（毎回textfieldの値を消すのがめんどくさい)
 *
 * Element.kindのenum化 もしくは クラス化(EPoint extends Element)
 * EPointとクラス化したらoverride w, hでサイズ変更できないようにする
 *
 * TODO-003
 * Ctrl-C Ctrl-Vの実装
 *
 * TODO-004
 * 名前にstage<ct>とかしてあれば<ct>の部分に
 * 自動的にカウントをしてくれたりすると便利
 *
 * TODO-005
 * リストのソートとかもあればうれぴー
 *
 * TODO-006
 * 長いこと使いそうになった時のみUndoの実装
 * いまいちやり方がわからない。
 * Effective JavaかオライリーのSwingかのどっちかに
 * Java版のUndoの実装があった気がするのでやる時は図書館で復習
 *
 * TODO-007 Menu.scala:64:chooserPath = Some(chooser.selectedFile)
 * フィルターの拡張子が足されない
 * testと入力して保存すると selectedFile はtestなのに
 * xml拡張子のフィルタがついてると実際には test.xml が生成されて困る
 * String.endWith(".xml")でなければ.xmlを追加するみたいな処理が望ましい
 * 
 * TODO-008
 * Labelとボタンの色指定が出来るようになるとうれしいってかしろ
 * 16進数を文字列をIntに当てはめる方法ははてぶにあった気がする
 *
 * TODO-009
 * 目印という機能前提のRECTがほしい
 * 保存のときデータxmlに吐き出さないでconfig.xmlのほうに吐き出す
 * レイヤーも別にしたいかも
 */

object Cursor {
  import java.awt.Cursor
  import java.awt.Cursor._

  val (arrow, move, hand) = (
    new Cursor(DEFAULT_CURSOR),
    new Cursor(MOVE_CURSOR), 
    new Cursor(HAND_CURSOR)
  )
}


/**
 * たいぷせーふじゃないメソッドのリフレクション（きり
 * 使用するときはじこせきにんで
 */
class Method(target:AnyRef, name:String, types:Class[_]*) {
  val method:Option[java.lang.reflect.Method] = {
    try {
      Some(target.getClass.getMethod(name, types:_*))
    } catch {
      case _:NoSuchMethodException =>
        val strTypes = if(types.isEmpty) "" else
          types.map { _.getSimpleName } mkString(", ")

        println("There is no public " +
                name + "(" + strTypes + ") " + 
                "method in the class " +
                target.getClass.getName())
        None
    }
  }

  def invoke(args:Any*):AnyRef = {
    method foreach { m => 
      try {
        return m.invoke(target, args.map(_.asInstanceOf[AnyRef]):_*)
      } catch {
        case e:Exception => e.printStackTrace
      }
    }
    return null
  }
}

object Message {
  def echo(any:Any) = println(any.toString)
}

object Main extends SimpleSwingApplication { 
  def separator:Panel = {
    new Panel {
      preferredSize = (2, 2)
      background = java.awt.Color.gray
    }
  }

  /**
   * default font
   */
  val font = new Font(Font.SANS_SERIF, Font.PLAIN, 11)
  //val font = new Font(Font.DIALOG, Font.BOLD, 9)

  private var _initialize = false
  private def initialize_=(b:Boolean) = _initialize = b 
  def initialize = _initialize

  def top = new MainFrame {
    title = "Main"
    resizable = false
    contents = new BorderPanel {
      import BorderPanel.Position._
      layout(new BorderPanel {
        layout(Menu.ui) = North
        layout(new BorderPanel {
          layout(separator) = South
          layout(Parts.ui) = Center
          layout(separator) = North
        }) = Center
        layout(Parameter.ui) = South
      }) = West
      layout(separator) = Center
      layout(Layer.ui) = East
    }
    Menu.initConfig

    centerOnScreen
    location = (location.x, 0)
    Editor.location = (location.x, size.height)

    Editor.visible = true
    initialize = true
  }
}
