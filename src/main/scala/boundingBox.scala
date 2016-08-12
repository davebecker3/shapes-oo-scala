package edu.luc.cs.laufer.cs372.shapes

import edu.luc.cs.laufer.cs372.shapes

// TODO: implement this behavior

object boundingBox {
  def apply(s: Shape): Location = s match {

    case Rectangle(width, height) => Location(0, 0, Rectangle(width, height))

    case Ellipse(half_width,half_height) => Location(0 - half_width, 0 - half_height,
      Rectangle(half_width * 2, half_height * 2))

    case Group(shapes @ _*) =>  {

      def getRight(s: Shape): Int = {
        val temp_loc = boundingBox(s)
        val temp_width = temp_loc.shape.asInstanceOf[Rectangle].width
        temp_width + temp_loc.x
      }

      def getBottom(s: Shape): Int = {
        val temp_loc = boundingBox(s)
        val temp_height = temp_loc.shape.asInstanceOf[Rectangle].height
        temp_height + temp_loc.y
      }

      val right = shapes.map(getRight(_)).max
      val bottom = shapes.map(getBottom(_)).max

      val min_x = shapes.map(boundingBox(_).x).min
      val min_y = shapes.map(boundingBox(_).y).min

      Location(min_x, min_y, Rectangle(right - min_x, bottom - min_y))

    }

    case Location(x,y,shape) => {
      val temp_loc = boundingBox(shape)
      val temp_x = temp_loc.x
      val temp_y = temp_loc.y
      Location(x + temp_x, y + temp_y, temp_loc.shape)
    }

    case _ => Location(0, 0, Rectangle(0, 0)) // not yet implemented
  }
}


