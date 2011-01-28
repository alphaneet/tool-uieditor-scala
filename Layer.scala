package neet.tool.editor.ui 
import editor._

import swing.Swing._
import swing._
import swing.event._

/**
 * Elementのselection管理は Layer.listView.selection がしている
 * selectionを変更する場合は必ずLayerを通して行う
 * EditorやParameterはLayerに通知してselectionを更新してもらってから
 * selectionの値を読み込むという形をとる
 * 
 * (例）
 * ・Editor上でマウスをクリック or ドラッグ   @see EditorPanel#reactions
 * ・Layerのリストビューをクリック or Ctrl-A  
 * ・PartsでボタンをクリックしてElementの生成 @see Parts @see Layer#+=
 * ↓
 * LayerのlistDataを更新 @see Layer#updateList
 * Editor,Parameterに通知　@see Layer#listView#reactions
 * ↓
 * Editorを更新 @see Editor#updateSelected
 * Parameterを更新 @see Parameter#updateText
 */
object Layer extends ComponentsCollection {
  me =>

  lazy val ui = new BorderPanel { 
  new ComponentsFactory(me) {
    import BorderPanel.Position._
    layout(label("-layer-")) = North
    layout(new BorderPanel {
      layout(
        new ScrollPane(listView){ preferredSize = (130, 0) }
      ) = Center
      border = EmptyBorder(2, 10, 2, 10)
    }) = Center

    layout(new BoxPanel(Orientation.Vertical) {
      List(
        List(button("up", "↑"), button("down", "↓")),
        List(button("remove"), button("clear"))
      ) foreach { c =>
        contents += new FlowPanel { contents ++= c } 
      }
    }) = South
  }}

  private val items = collection.mutable.ArrayBuffer[Element]()

  private val listView = new ListView(items) {
    listenTo(selection)
    reactions += {
      case _:SelectionChanged => 
        Editor.updateSelected
        Parameter.updateText 
    }
  }

  def elements = items.iterator
  def selected = listView.selection.items.iterator

  /**
   * listDataを更新すると選択項目が初期化されてしまうので
   * 更新する前の値をコピーして更新後再びselectionに追加している
   * 
   * リストの数を変更した後（追加や削除）に行うと
   * idxとitemsの数が違うので行ってはいけない
   * 
   * 基本的には（複数の）Elementのパラメーターを更新した場合に行う
   * @see Parameter#updateClicked
   */
  def updateList {
    val idx = listView.selection.indices
    val prev = idx.clone
    listView.listData = items
    prev foreach(idx +=)
  }
  def updateList(e:Element, shouldScroll:Boolean=true) {
    listView.listData = items
    listView.peer.setSelectedValue(e, shouldScroll)
  }
  def updateList(elist:List[Element]) {
    listView.listData = items
    elist foreach { e =>
      listView.selection.indices += items.indexOf(e) 
    }
  }

  def clearSelection = listView.peer.clearSelection

  def upClicked =
    Dialog.showMessage(ui, "まだ実装してないおっおっ")

  def downClicked =
    Dialog.showMessage(ui, "まだ実装してないおっおっ")

  def checkDialog:Boolean = {
    import Dialog._
    (showConfirmation(ui, "削除してもいいですか？", "確認ウィンドウ")
     == Result.Yes)
  }
  def removeClicked {
    if(selected.isEmpty) return
    if(checkDialog) removeSelected
  }
  def removeSelected = listView.selection.items.foreach(-=)

  def clearClicked = if(checkDialog) clearElements

  def clearElements {
    items.clear
    Editor.clear
    listView.listData = items
  }

  def +=(e:Element):this.type = { 
    Editor += e
    items += e 
    updateList(e)
    this 
  }
  def -=(e:Element):this.type = { 
    Editor -= e
    items -= e 
    if(items.isEmpty) listView.listData = items
    else updateList(items.last)
      
  //  listView.peer.setSelectedValue(listView., true)
    this 
  }
}
