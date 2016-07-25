package edu.luc.cs.laufer.cs372.shapes

// TODO: implement this behavior

object boundingBox {
  def apply(s: Shape): Location = s match {
    case Rectangle(width, height) => Location(0, 0, Rectangle(width, height))
    case Ellipse(half_width,half_height) => Location(0 - half_width, 0 - half_height,
      Rectangle(half_width * 2, half_height * 2))
    case Group(_*) => Location(3, 3, Rectangle(3, 3))
    case Location(x,y,shape) => Location(x, y, boundingBox(shape).shape)

    case _ => Location(0, 0, Rectangle(0, 0)) // not yet implemented
  }
}
