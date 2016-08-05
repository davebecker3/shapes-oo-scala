package edu.luc.cs.laufer.cs372.shapes

// TODO: implement this behavior

object boundingBox {
  def apply(s: Shape): Location = s match {
    case Rectangle(width, height) => Location(0, 0, Rectangle(width, height))
    case Ellipse(half_width,half_height) => Location(0 - half_width, 0 - half_height,
      Rectangle(half_width * 2, half_height * 2))

    case Group(shapes @ _*) =>  Location(shapes.map({
      boundingBox(_).x
    }).min, 0,Rectangle(0,0))


    case OldGroup(shapes @ _*) =>  Location({
      val xs = Set.newBuilder[Int]
      val ys = Set.newBuilder[Int]
      val ws = Set.newBuilder[Int]
      val hs = Set.newBuilder[Int]
      for (s <- shapes){
        val temp_loc = boundingBox(s)
        xs.+=(temp_loc.x)
        ys.+=(temp_loc.y)
        val shape = temp_loc.shape
        shape match{
          case Rectangle(width @ _, height @ _) => {
            ws +=(width)
            hs +=(height)
          }
          case _ => println("WTF?")
        }

        val fin_x = xs.result().min
        val fin_y = ys.result().min
        val fin_w = ws.result().max
        val fin_h = hs.result().max

        val fin_loc = Location(fin_x, fin_y, Rectangle(fin_w, fin_h))

      }



    })



    case Location(x,y,shape) => Location(x, y, boundingBox(shape).shape)

    case _ => Location(0, 0, Rectangle(0, 0)) // not yet implemented
  }
}
