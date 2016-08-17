package edu.luc.cs.laufer.cs372.shapes

import edu.luc.cs.laufer.cs372.shapes

// File copied from "size.scala".
// Copied function "incBy" from "orgchartGeneric.sc" in "misc-explorations-scala".

object scale {

  def incBy(perc: Float)(num: Int): Int = scala.math.round(num.toFloat * (100 + perc) / 100)

  def apply(s: Shape, perc: Float): Shape = s match {

    case Rectangle(width, height) => Rectangle(
      incBy(perc)(width),
      incBy(perc)(height)
    )

    case Ellipse(half_width, half_height) => Ellipse(
      incBy(perc)(half_width),
      incBy(perc)(half_height)
    )

    case Location(x, y, shape) => Location(
      incBy(perc)(x),
      incBy(perc)(y),
      scale(shape, perc)
    )

    case Group(shapes @ _*) => Group {
      // I can't figure out how to populate the returned Group object with the modified contents of the
      // received Group. If I iterate through the contents of the received Group using ".map()",
      // it returns a Seq[Shape], which the constructor won't accept, and if I use ".foreach()" or a "for"
      // comprehension, the return type is Unit, which doesn't work eiher.

      println("Function \"scale\" received an object of type \"Group\", which I don't know how to process.")
      println(shapes)
      val n = shapes.map(scale(_, perc))
      n.toArray
     // shapes


        Rectangle(100, 200)

      //  Rectangle(0, 0)
    }

    case _ => {
      println("Function \"scale\" received object of unknown type.")
      Rectangle(0,0)
    }
  }
}



