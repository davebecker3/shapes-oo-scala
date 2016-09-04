package edu.luc.cs.laufer.cs372.shapes

import edu.luc.cs.laufer.cs372.shapes

// TODO: implement this behavior

object boundingBox {
  def apply(s: Shape): Location = s match {

    case Rectangle(width, height) => Location(0, 0, Rectangle(width, height))

    case Ellipse(half_width,half_height) => Location(0 - half_width, 0 - half_height,
      Rectangle(half_width * 2, half_height * 2))

    case Location(x,y,shape) => {
      val temp_loc = boundingBox(shape)
      val temp_x = temp_loc.x
      val temp_y = temp_loc.y
      Location(x + temp_x, y + temp_y, temp_loc.shape)
    }

    case Group(shapes @ _*) =>  {
      /*
      // =============================================================
      // This was my first implementation, which works, but is
      // inefficient. I couldn't figure out how to use ".map()" to
      // obtain more than one datum at a time, so I resorted to four
      // consecutive traversals of the data structure to obtain the
      // information necessary to construct the proper Location. I
      // defined nested functions in an attempt to keep things tidy.
      // =============================================================

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
      // =============================================================
      */



      // =============================================================
      // This is my second attempt. After reading this page:
      //
      // http://www.artima.com/pins1ed/for-expressions-revisited.html
      //
      // I was able to figure out how to use a "for" comprehension
      // with "yield" to extract all the necessary data in a single
      // traversal.
      // =============================================================

      val boundingBoxResults = {
       for {
         sh <- shapes
         loc = boundingBox(sh)
         x = loc.x
         y = loc.y
         width = loc.shape.asInstanceOf[Rectangle].width
         height = loc.shape.asInstanceOf[Rectangle].height
         right = (width + x)
         bottom = (height + y)
       } yield (x, y, right, bottom)
      }

      val minX = boundingBoxResults.map(_._1).min
      val minY = boundingBoxResults.map(_._2).min
      val maxRight = boundingBoxResults.map(_._3).max
      val maxBottom = boundingBoxResults.map(_._4).max

      Location(minX, minY, Rectangle(maxRight - minX, maxBottom - minY))

      // println("■" + " ;-) ")
      // =============================================================

    }

    case _ => Location(0, 0, Rectangle(0, 0)) // not yet implemented
  }
}


