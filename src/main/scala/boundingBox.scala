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

      Rectangle(

        {val groupMinX = shapes.map(boundingBox(_).x).min

          shapes.map({
        def max_x(minX: Int, s: Shape): Int = {
          val temp_loc = boundingBox(s)
          val temp_x = temp_loc.x
          val temp_width = temp_loc.shape.asInstanceOf[Rectangle].width
          if (temp_x == minX) temp_width else (temp_width - minX)
        }

        max_x(groupMinX, _)
      }).max}


        ,0)

    )

    case Location(x,y,shape) => Location(x, y, boundingBox(shape).shape)

    case _ => Location(0, 0, Rectangle(0, 0)) // not yet implemented
  }
}
