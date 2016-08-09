package edu.luc.cs.laufer.cs372.shapes

// TODO: implement this behavior

object boundingBox {
  def apply(s: Shape): Location = s match {
    case Rectangle(width, height) => Location(0, 0, Rectangle(width, height))
    case Ellipse(half_width,half_height) => Location(0 - half_width, 0 - half_height,
      Rectangle(half_width * 2, half_height * 2))

    case Group(shapes @ _*) =>  Location(

      shapes.map(boundingBox(_).x).min,
      shapes.map(boundingBox(_).y).min,

      Rectangle(shapes.map({
        def max_x(s: Shape): Int = {
          val location = boundingBox(s)
          val x = location.x
          val w = location.shape.asInstanceOf[Rectangle].width
          val absoluteWidth = x - w
          absoluteWidth
        }
        max_x(_)
      }).max


        ,0)

    )

    case Location(x,y,shape) => Location(x, y, boundingBox(shape).shape)

    case _ => Location(0, 0, Rectangle(0, 0)) // not yet implemented
  }
}
