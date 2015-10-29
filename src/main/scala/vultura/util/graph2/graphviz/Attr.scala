package vultura.util.graph2.graphviz

sealed trait Attr {
  def dotString: String
}
sealed trait NAttr extends Attr
sealed trait EAttr extends Attr

case class Label(text: String) extends NAttr with EAttr {
  override def dotString: String = s"""label="$text""""
}

case class Color(colorName: String) extends NAttr with EAttr {
  override def dotString: String = s"color=$colorName"
}

object Color {
  val RED = Color("red")
  val BLACK = Color("black")
  val BLUE = Color("blue")
  val GREEN = Color("green")
  val DARK_RED = Color("dark_red")
  val DARK_BLUE = Color("dark_blue")
  val CORAL = Color("coral")
  val ORANGE = Color("orange")
  val TEAL = Color("teal")
}

sealed trait Shape extends NAttr
object Shape {
  object ELLIPSE extends Shape { override def dotString: String = "ellipse"}
  object OVAL extends Shape { override def dotString: String = "oval" }
  object CIRCLE extends Shape { override def dotString: String = "circle"}
  object EGG extends Shape { override def dotString: String = "egg"}
  object TRIANGLE extends Shape { override def dotString: String = "triangle"}
  object BOX extends Shape { override def dotString: String = "box"}
  object TRAPEZIUM extends Shape { override def dotString: String = "trapezium"}
  object HOUSE extends Shape { override def dotString: String = "house"}
}
