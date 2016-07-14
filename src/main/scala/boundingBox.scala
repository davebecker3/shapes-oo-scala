package edu.luc.cs.laufer.cs372.shapes

// TODO: implement this behavior

object boundingBox {
  def apply(s: Shape): Location = s match {
    case Rectangle(_,_) => Location(1, 1, Rectangle(1, 1))
    case Ellipse(_,_) => Location(2, 2, Rectangle(2, 2))
    case Group(_*) => Location(3, 3, Rectangle(3, 3))
    case Location(_,_,_) => Location(4, 4, Rectangle(4, 4))

    case _ => Location(0, 0, Rectangle(0, 0)) // not yet implemented
  }
}
