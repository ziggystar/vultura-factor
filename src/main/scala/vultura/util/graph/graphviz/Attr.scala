package vultura.util.graph.graphviz

sealed trait Attr {
  def name: String
  def value: String
  def dotString: String = s"$name=$value"
}
/** Node attribute. */
sealed trait NAttr extends Attr
/** Edge attribute.*/
sealed trait EAttr extends Attr
/** Graph attribute. */
sealed trait GAttr extends Attr

case class Label(text: String) extends NAttr with EAttr {
  override def name: String = "label"
  override def value: String = s""""$text""""
}

case class Color(colorName: String) extends NAttr with EAttr {
  override def name: String = "color"
  override def value: String = colorName
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

sealed trait Shape extends NAttr {
  override def name: String = "shape"
}
object Shape {
  object ELLIPSE extends Shape { override def value: String = "ellipse"}
  object OVAL extends Shape { override def value: String = "oval" }
  object CIRCLE extends Shape { override def value: String = "circle"}
  object EGG extends Shape { override def value: String = "egg"}
  object TRIANGLE extends Shape { override def value: String = "triangle"}
  object BOX extends Shape { override def value: String = "box"}
  object TRAPEZIUM extends Shape { override def value: String = "trapezium"}
  object HOUSE extends Shape { override def value: String = "house"}
}
