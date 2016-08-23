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

    case Group(shapes @ _*) => Group(shapes.map(scale(_, perc)): _*)
      // http://stackoverflow.com/questions/7040382/scala-constructor-taking-either-seq-or-varargs

    case _ => {
      println("Function \"scale\" received object of unknown type.")
      Rectangle(0,0)
    }
  }
}



