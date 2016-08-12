package edu.luc.cs.laufer.cs372.shapes

import edu.luc.cs.laufer.cs372.shapes

// File copied from "boundingBox.scala".
// Function based on copy of size function "size" in "orgchart.sc" in "misc-explorations-scala".

object size {
  def apply(s: Shape): Int = s match {
    case Rectangle(_, _) => 1
    case Ellipse(_, _) => 1
    case Location(_, _, shape @ _) => size(shape)
    case Group(shapes @ _*) => shapes.map(size(_)).sum
    case _ => {println("Function \"size\" received object of unknown type."); 0}
  }
}


