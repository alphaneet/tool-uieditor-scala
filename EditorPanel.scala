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
 * Editorの描画を担当
 *
 * @see Editor
 */
private[editor] object EditorPanel extends Panel 
                              with SequentialContainer.Wrapper 
{
  import Editor.components

  override lazy val peer = 
    new javax.swing.JPanel(null) with SuperMixin

  background = Color.white

  /**
   * Wait     - 待ち
   * Selected - Componentが選択されているとき
   * Search   - マウスでドラッグしてComponentを選択しているとき
   */
  object ActionMode extends Enumeration {
    type Action = Value
    val Wait, Selected, Search = Value
  }
  import ActionMode._

  /**
   * ActionMode.Selected時の状態
   * Single 単体を選択している状態
   * Multi 複数を選択している状態
   *
   * ・Single と Multiの違い
   * ActionMode.Selected中にクリックした場合
   * -Single 一番小さい面積のComponentを選ぶ直す
   * -Multi  範囲外をクリックしない限り再選択は行われない
   */
  object IntervalMode extends Enumeration {
    type Interval = Value
    val Single, Multi = Value
  }
  import IntervalMode._

  abstract class Rect {
    var x, y, w, h = 0
    def pos = new Point(x, y)
    def dim = new Dimension(w, h)
    def rect = new Rectangle(x, y, w, h)
    def isHit(p:Point) =
      (p.x > x && p.x < x + w && p.y > y && p.y < y + h)
    def clear {
      x = 0; y = 0; w = 0; h = 0;
    }
    def draw(g:Graphics2D)
  }

  var searchRect = new Rect {
    def draw(g:Graphics2D) {
      g.setColor(new Color(127, 127, 255, 100))
      g.fillRect(x, y, w, h)
      g.setColor(new Color(127, 127, 255))
      g.drawRect(x, y, w, h)
    }
  }
  var selectedRect = new Rect {
    var start = new Point
    /**
     * MouseDragged後のMouseReleased時や
     * Key入力の後などの移動処理が終わった後に呼び出す
     */
    def updateStart {
      start.x = x
      start.y = y
    }
    def draw(g:Graphics2D) {
      g.setColor(new Color(255, 184, 43))
      g.drawRect(x, y, w-1, h-1)
      g.drawRect(x-1, y-1, w+1, h+1)
    }
  }

  var action:Action = Wait
  var interval:Interval = Single
  var pressed:Point = new Point
  var selectedComponents = 
    collection.mutable.Map[Element, Component with Ex]()
  var wgrid, hgrid:Option[Int] = None

  /**
   * @return pにヒットしている面積の一番小さいElement
   */
  def hitElement(p:Point):Option[Element] = {
    val hits = components.filter(_._2.isHit(p)).map(_._1)

    if(hits.isEmpty) return None

    // 複数が選択範囲に入っていた場合面積が一番小さいのを選択する
    def area(e:Element) = e.w * e.h
    Some(hits.reduceLeft { (e1, e2) =>
      if(area(e1) < area(e2)) e1 else e2
    })
  }

  /**
   * @param p 移動先の座標
   */
  def moveSelectedComponents(p:Point) {
    selectedComponents foreach { map =>

      val (elem, comp) = map
      comp.location = (p.x - comp.diff.x, p.y - comp.diff.y)
      elem.x = comp.location.x 
      elem.y = comp.location.y
    }
    selectedRect.x = p.x
    selectedRect.y = p.y
    Parameter.updateText
  }

  def updateSelected {
    if(Layer.selected.isEmpty) {
      selectedRect.clear
      return
    }

    val first  = Layer.selected.next
    var (sx, sy) = (first.x, first.y) // 左上の座標
    var (ex, ey) = (first.w + sx, first.h + sy) // 右下の座標

    Layer.selected foreach { e =>
      if(e.x < sx) sx = e.x
      if(e.y < sy) sy = e.y

      if(e.x + e.w > ex) ex = e.x + e.w
      if(e.y + e.h > ey) ey = e.y + e.h
    }

    selectedComponents = components filter { map =>
      val (elem, comp) = map
      val ret = Layer.selected.exists(_ == elem)
      if(ret) {
        comp.diff.x = sx - comp.location.x
        comp.diff.y = sy - comp.location.y
      }
      ret
    }

    selectedRect.start.x = sx; 
    selectedRect.start.y = sy; 
    selectedRect.x = sx; 
    selectedRect.y = sy
    selectedRect.w = ex - sx; 
    selectedRect.h = ey - sy

    interval = if(Layer.selected.length == 1) Single else Multi

    if(action != Search) action = Selected
    repaint
  }

  override def paint(g:Graphics2D) {
    super.paint(g)

    g.setColor(new Color(0, 0, 0, 32))
    wgrid foreach { w =>
      val xmax = size.width / w
      for(i <- 0 until xmax) {
        g.drawLine(i*w, 0, i*w, size.height)
      }
    }
    hgrid foreach { h =>
      val ymax = size.height / h
      for(i <- 0 until ymax) {
        g.drawLine(0, i*h, size.width, i*h)
      }
    }

    def drawHitElements {
      g.setColor(new Color(127, 255, 127))
      Layer.selected foreach { e =>
        val (x, y, w, h) = e.rect
        g.drawRect(x, y, w-1, h-1)
        g.drawRect(x-1, y-1, w+1, h+1)
      }
    }
    action match {
      case Wait =>
      case Search =>
        drawHitElements
        selectedRect.draw(g)
        searchRect.draw(g)

      case Selected =>
        drawHitElements
        selectedRect.draw(g)
    }
  }

  focusable = true
  listenTo(mouse.clicks, mouse.moves, keys)
  reactions += {
    case e:MousePressed => 
      pressed.x = e.point.x 
      pressed.y = e.point.y 

      /**
       * ヒットしてるコンポネントがあれば 
       * Action.Selected なければ Action.Search に
       *
       * @see updateSelected
       */
      def gotoSelectedOrSearch {
        val hit = hitElement(e.point)
        if(hit.isDefined) {
          Layer updateList hit.get
          action = Selected
        } else {
          Layer.clearSelection
          action = Search
        }
        repaint
      }

      action match {
        case Wait => gotoSelectedOrSearch  
        case Selected => interval match {
          case Single => gotoSelectedOrSearch 
          case Multi  => 
            if(!selectedRect.isHit(e.point)) gotoSelectedOrSearch 
        }
      }

    case e:MouseDragged => 
      action match {
        case Selected => 
          var x = pressed.x + (e.point.x - pressed.x) -
                 (pressed.x - selectedRect.start.x)
          var y = pressed.y + (e.point.y - pressed.y) -
                 (pressed.y - selectedRect.start.y)

          wgrid foreach { w =>
            x = scala.math.round((x.toFloat/(w>>1)) / 2).toInt * w
          }
          hgrid foreach { h =>
            y = scala.math.round((y.toFloat/(h>>1)) / 2).toInt * h
          }
          moveSelectedComponents((x, y))

        case Search =>
          val p = e.point
          val (dx, dy) = (p.x - pressed.x, p.y - pressed.y)
          val r = searchRect
          r.w = scala.math.abs(dx)
          r.h = scala.math.abs(dy)
          r.x = if(dx > 0) pressed.x else pressed.x - r.w
          r.y = if(dy > 0) pressed.y else pressed.y - r.h

          /**
           * TODO:毎回更新しすぎかも
           * 前回と違うなら更新するとかしたほうがいいかも
           */
          Layer.updateList(components.filter {
            _._2.isHit(searchRect.rect) 
          } map { _._1 } toList)

        case Wait =>
          // 画面の端っこの微妙な部分をクリックして
          // MousePressedが呼ばれないでWaitモードのまま
          // MouseDraggedに来てしまうことが稀によくあるっぽいので
          // 空のCaseを用意。基本的にはWaitのままDraggedにくるのはバグ
          // (かならずSelectedかSearchになっている・・・ﾊｽﾞ）
      }
      repaint

    case e:MouseMoved => 
      def isHitComp = components.exists(_._2.isHit(e.point))
      action match {
        case Wait => 
          if(isHitComp) cursor = Cursor.move
          else cursor = Cursor.arrow

        case Selected =>
          if(selectedRect.isHit(e.point) || isHitComp) {
            cursor = Cursor.move
          } else cursor = Cursor.arrow
      }

    case e:MouseReleased => 
      action match {
        case Search =>
          action = if(Layer.selected.isEmpty) Wait else Selected 
          updateSelected
          searchRect.clear
          repaint

        case Selected =>
          selectedRect.updateStart

       case _ => 
      }

    case KeyPressed(_, Key.Delete, _, _) => Layer.removeSelected
    case KeyPressed(_, Key.G, _, _) => Menu.gridClicked
    case KeyPressed(_, key, Key.Modifier.Control, _) =>
      CrossKeyMap find {
        key == _._1
      } foreach { e =>
        Menu.addGridSize(e._2)
      }
    case KeyPressed(_, key, _, _) =>
      if(!selectedComponents.isEmpty) {
        CrossKeyMap find { 
          key == _._1
        } foreach { m =>
          val (mx, my) = m._2
          val pos = selectedRect
          moveSelectedComponents(pos.x + mx, pos.y + my)
          selectedRect.updateStart
          repaint
        }
      }
  }
}
