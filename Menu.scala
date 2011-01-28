package neet.tool.editor.ui 
import editor._

import swing.Swing._
import swing._
import swing.event._
import java.io.File

object Menu extends ComponentsCollection { 
  me =>

  lazy val ui = new BoxPanel(Orientation.Vertical) { 
  new ComponentsFactory(me) {
    List(
      List(label("-menu-")),
      List(textfield("width"), label("x"),
           textfield("height"), button("resize")),
      List(textfield("wgrid"), label("x"), 
           textfield("hgrid"), button("grid")),
      List(button("save"), button("load"), button("help"))
    ) foreach { c =>
      contents += new FlowPanel { contents ++= c }
    }
  }}
  private var chooserPath:Option[File] = None
  private var gridVisible = false
  private val CONFIG_PATH = "config.xml"

  // configファイルがあれば開く
  def initConfig {
    try {
      val config = xml.XML.loadFile(CONFIG_PATH)
      val path = (config \ "path").text
      if(path != "") chooserPath = Some(new File(path))

      text("wgrid") = (config \ "wgrid").text
      text("hgrid") = (config \ "hgrid").text
      gridClicked
    } catch {
      case _:java.io.FileNotFoundException =>
    }

    resizeClicked
  }

  def fileChooser(dialog:(FileChooser) => FileChooser.Result.Value)
    :Option[File] = 
  {
    val chooser = new FileChooser(chooserPath.getOrElse(null))
    chooser.fileFilter = 
      new javax.swing.filechooser.FileNameExtensionFilter("XMLファイル", "xml")

    val ret = dialog(chooser)
    if(ret == FileChooser.Result.Approve) {
      chooserPath = Some(chooser.selectedFile)
      chooserPath
    } else None
  }

  def resizeClicked {
    val (w, h) = (Menu.toInt("width").getOrElse(200),
      Menu.toInt("height").getOrElse(200))

    Menu.text("width") = w; Menu.text("height") = h
    Editor.size = (w, h)
  }

  def saveClicked = fileChooser(_.showSaveDialog(ui)).foreach(save)
  def loadClicked = fileChooser(_.showOpenDialog(ui)).foreach(load)
  def helpClicked = Help.open

  def addGridSize(dim:Dimension) {
    var (wgrid, hgrid) = (Menu.toInt("wgrid"), Menu.toInt("hgrid"))
    Menu.text("wgrid") = wgrid.getOrElse(0) + dim.width
    Menu.text("hgrid") = hgrid.getOrElse(0) + dim.height
    gridClicked
  }
  def gridClicked {
    var (wgrid, hgrid) = (Menu.toInt("wgrid"), Menu.toInt("hgrid"))

    if(wgrid == Editor.wgrid && hgrid == Editor.hgrid) {
      gridVisible  = !gridVisible
      if(gridVisible) Editor.grid(wgrid, hgrid)
      else Editor.grid(None, None)

    } else {
      gridVisible = true
      Editor.grid(wgrid, hgrid)
    }
  }

  /**
   * 名前と拡張子を分ける拡張子が空の場合はextをかえすを返す
   */
  def splitNameExt(file:File, ext:String):(String, String) = {
    val parent = file.getParent + File.separator
    val name = file.getName
    val idx = name.lastIndexOf(".")

    if(idx == -1) (parent + name, "." + ext) else {
      (parent + name.substring(0, idx), name.substring(idx))
    }
  }

  def save(file:File) {
    val (name, ext) = splitNameExt(file, "xml")
    var ct = -1
    val elems = Layer.elements map { e =>
      ct += 1

<element>
  <id>{ct}</id>
  <kind>{e.kind}</kind>
  <name>{e.name}</name>
  <text>{e.name}</text>
  <x>{e.x}</x><y>{e.y}</y>
  <w>{e.w}</w><h>{e.h}</h>
  <size>{e.size}</size>
</element>
    }

    val ui = 
<ui>
<width>{Editor.size.width}</width>
<height>{Editor.size.height}</height>
{elems}
</ui>

    val config = 
<config>
  <wgrid>{Editor.wgrid.getOrElse("")}</wgrid>
  <hgrid>{Editor.hgrid.getOrElse("")}</hgrid>
  <path>{chooserPath.getOrElse("")}</path>
</config>

    xml.XML.save(name+ext, ui, "utf-8")
    xml.XML.save(CONFIG_PATH, config, "utf-8")
  }

  def load(file:File) {
    Layer.clearElements

    val xml = scala.xml.XML.loadFile(file.getAbsolutePath)
    text("width")  = (xml \ "width").text
    text("height") = (xml \ "height").text
    resizeClicked

    val param = Parameter.text
    (xml \ "element").foreach { element =>
      def e(name:String) = (element \ name).text
      param("name") = e("name")
      param("text") = e("text")

      param("x") = e("x")
      param("y") = e("y")
      param("w") = e("w")
      param("h") = e("h")
      param("size") = e("size")

      Parts.create(e("kind"))
    }
  }
}

object Help extends Dialog {
  title = "help"
  resizable = false
  val xml = 
<html>
  <head><title>キーボード操作一覧</title></head>

  <h2>エディタウィンドウ操作時</h2>
  <p>上下左右: 選択中パーツの移動</p>
  <p>Ctrl + 上下左右: グリッドのサイズを一変更</p>
  <p>Delete: 選択中のパーツの削除</p>

  <h2>パラメーター入力時</h2>
  <p>Enter: パラメーター変更の決定</p>
</html>

  contents = new Label(xml.toString) {
    horizontalAlignment = Alignment.Left
    verticalAlignment   = Alignment.Top
    border = Swing.EmptyBorder(5, 5, 5, 5)
  }

//  contents = new TabbedPane {
//    List("help", "shortcut") foreach { htmlName =>
//      pages += new TabbedPane.Page(htmlName, new Label {
//        opaque = true
//        horizontalAlignment = Alignment.Left
//        verticalAlignment   = Alignment.Top
//        border = Swing.EmptyBorder(5, 5, 5, 5)
//        background = java.awt.Color.white
//        try {
//          text = xml.XML.loadFile(htmlName + ".html").toString
//        } catch {
//          case _:java.io.FileNotFoundException => 
//            Dialog.showMessage(this, htmlName + ".html 忘れてまつよ")
//        }
//      })
//    }
//  }
}
