package neet.tool.editor.ui 
package test

/**
 * TODO: scalaTestのやり方を勉強する
 */
object Test {
  def main(args:Array[String]) {
    Main.main(args)
    while(!Main.initialize) Thread.sleep(3)

    Parameter.text("x") = 30
    Parameter.text("y") = 120
    Parts.labelClicked

    Parameter.text("x") = 130
    Parameter.text("y") = 100
    Parts.buttonClicked

    Parameter.text("x") = 10
    Parameter.text("y") = 20
    Parameter.text("w") = 100
    Parts.textfieldClicked

    Parameter.text("x") = 60
    Parameter.text("y") = 60
    Parameter.text("w") = 100
    Parameter.text("h") = 100
    Parts.rectClicked

    Parameter.text("x") = 100
    Parameter.text("y") = 50
    Parts.pointClicked

    // TODO: listViewをprivateにしてしまったからアクセスできにい
    // Test時だけprivateにアクセスできる方法があるか調べる
    // さらっと読んだClojureの入門本の冒頭にClojureはテスト時にだけ
    // privateにもアクセスできるようにできます（キリ　
    // みたいなこと書いてた気がするので今度見てみる
//    val l = Layer.listView
//    //for(i <- 1 until l.listData.length-1) {
//    for(i <- 1 until 2) {
//      l.selection.indices += i
//    }

    Menu.text("wgrid") = 16
    Menu.text("hgrid") = 32
    Menu.gridClicked
  }
}
