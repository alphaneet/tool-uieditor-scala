package neet.tool.editor.ui 

/**
 * default parameter object
 */
object Element { 
  val name, text = ""
  val (x, y, w, h, size) = (0, 0, 60, 20, 10)
  override def toString = ""
}

class Element(val kind:String) {
  var _name:Option[String] = None
  def name:String = _name.getOrElse(Element.name)
  def name_=(v:String) = _name = if(v=="") None else Some(v)

  var _text:Option[String] = None
  def text:String = _text.getOrElse(Element.text)
  def text_=(v:String) = _text = if(v=="") None else Some(v)

  /**
   * create script
   * <code>
   * val kata = "Int" 
   * val (l, r) = ("{", "}")
   * List("x", "y", "w", "h", "size") foreach { v =>
   *   val upper = 
   *     v.substring(0,1).toUpperCase + v.substring(1) 
   *   val code = <code>
   * var _{v}:Option[{kata}] = None
   * def {v}:{kata} = _{v}.getOrElse(Element.{v})
   * def {v}_=(v:{kata}) {l} {v} = Some(v) {r}
   * def {v}_=(v:Option[{kata}]) {l} if(v.isDefined) _{v} = v {r}</code>
   * 
   *   println(code.text)
   * }
   * </code>
   */
  var _x:Option[Int] = None
  def x:Int = _x.getOrElse(Element.x)
  def x_=(v:Int) { x = Some(v) }
  def x_=(v:Option[Int]) { if(v.isDefined) _x = v }

  var _y:Option[Int] = None
  def y:Int = _y.getOrElse(Element.y)
  def y_=(v:Int) { y = Some(v) }
  def y_=(v:Option[Int]) { if(v.isDefined) _y = v }

  var _w:Option[Int] = None
  def w:Int = _w.getOrElse(Element.w)
  def w_=(v:Int) { w = Some(v) }
  def w_=(v:Option[Int]) { if(v.isDefined) _w = v }

  var _h:Option[Int] = None
  def h:Int = _h.getOrElse(Element.h)
  def h_=(v:Int) { h = Some(v) }
  def h_=(v:Option[Int]) { if(v.isDefined) _h = v }

  var _size:Option[Int] = None
  def size:Int = _size.getOrElse(Element.size)
  def size_=(v:Int) { size = Some(v) }
  def size_=(v:Option[Int]) { if(v.isDefined) _size = v }

  def textOrElse(default:String):String = {
    _text.orElse(_name).getOrElse(default)
  }

  def pos = (x, y)
  def dim = (w, h)
  def rect = (x, y, w, h)

  /**
   * create script
   * <code>
   * List("name", "text", "x", "y", "w", "h", "size") map { 
   *   e => """"%s=" + _%s.getOrElse("None")""".format(e,e) 
   * } mkString(" + \" \"\n")
   * </code>
   */
  def dump {
    println(kind + " " +
            "name=" + _name.getOrElse("None") + " " +
            "text=" + _text.getOrElse("None") + " " +
            "x=" + _x.getOrElse("None") + " " +
            "y=" + _y.getOrElse("None") + " " +
            "w=" + _w.getOrElse("None") + " " +
            "h=" + _h.getOrElse("None") + " " +
            "size=" + _size.getOrElse("None"))
  }

  /**
   * リストに表示される文字列になる
   */
  override def toString = name + " : " + kind
}
