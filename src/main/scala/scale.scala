package edu.luc.cs.laufer.cs372.shapes

import edu.luc.cs.laufer.cs372.shapes

// File copied from "size.scala".
// Function based on function "incBy" in "orgchartGeneric.sc" in "misc-explorations-scala".

object scale {

  def incBy(perc: Float)(num: Int): Int = scala.math.round(num.toFloat * (100 + perc) / 100)

  def apply(perc: Float)(s: Shape): Shape = s match {
    case Rectangle(width, height) => Rectangle((incBy(perc)(width)), (incBy(perc)(height)))
    case Ellipse(half_width, half_height) => Ellipse((incBy(perc)(half_width)), (incBy(perc)(half_height)))
    case Location(x, y, shape @ _) => Location((incBy(perc)(x)), (incBy(perc)(y)), scale(perc)(shape))
    case Group(shapes @ _*) => Group(shapes.map(scale(perc)(_)))
    case _ => {println("Function \"scale\" received object of unknown type."); Rectangle(0,0)}
  }
}



