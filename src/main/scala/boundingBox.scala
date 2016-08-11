package edu.luc.cs.laufer.cs372.shapes

import edu.luc.cs.laufer.cs372.shapes

// TODO: implement this behavior

object boundingBox {
  def apply(s: Shape): Location = s match {

    case Rectangle(width, height) => Location(0, 0, Rectangle(width, height))

    case Ellipse(half_width,half_height) => Location(0 - half_width, 0 - half_height,
      Rectangle(half_width * 2, half_height * 2))

    case Group(shapes @ _*) =>  {

      def groupLoc(shape_seq: Seq[Shape]): Location = {

        val exes: List[Int] = List()
        val whys: List[Int] = List()
        val rights: List[Int] = List()
        val bottoms: List[Int] = List()

        val widths: List[Int] = List()
        val heights: List[Int] = List()

        for (s <- shape_seq){
          val temp_loc = boundingBox(s)
          val temp_width = temp_loc.shape.asInstanceOf[Rectangle].width
          val temp_height = temp_loc.shape.asInstanceOf[Rectangle].height

          exes.::(temp_loc.x)
          whys.::(temp_loc.y)

          rights.::(temp_loc.x + temp_width)
          bottoms.::(temp_loc.y + temp_height)
        }

        Location(exes.min, whys.min, Rectangle(rights.max - exes.min, bottoms.max - whys.min))
      }

      groupLoc(shapes)

    }

    case Location(x,y,shape) => Location(x, y, boundingBox(shape).shape)

    case _ => Location(0, 0, Rectangle(0, 0)) // not yet implemented
  }
}


/*

case Group(shapes @ _*) =>  Location(

      shapes.map(boundingBox(_).x).min,
      shapes.map(boundingBox(_).y).min,

      Rectangle(

        {val groupMinX = shapes.map(boundingBox(_).x).min

          shapes.map({
        def max_x(minX: Int, s: Shape): (Int, Int) = {
          val temp_loc = boundingBox(s)
          val temp_x = temp_loc.x
          val temp_width = temp_loc.shape.asInstanceOf[Rectangle].width
          if (temp_x == minX) (temp_width, 0) else ((temp_width - minX), 0)
        }

        max_x(groupMinX, _)
      }).max})

    )

    ===

        case Group(shapes @ _*) =>  Location(
      {def returnLocation() = {
        val tempTuple = (0, 0, Rectangle(0,0))
        tempTuple}
        returnLocation()
      })



 */