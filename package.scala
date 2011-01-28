package neet.tool.editor
import swing.event.Key

package object ui {
  // debug only
  def p(x:Any) = println(x)
  def pp(x:Any) = print(x)
  def pf(text:String, xs:Any*) = println(format(text, xs:_*))
  def ppf(text:String, xs:Any*) = print(format(text, xs:_*))

  val CrossKeyMap = 
    Map(Key.Up   -> (0, -1), Key.Left  -> (-1, 0),
        Key.Down -> (0,  1), Key.Right -> ( 1, 0))
}
