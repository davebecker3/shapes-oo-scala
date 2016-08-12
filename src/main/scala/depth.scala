package edu.luc.cs.laufer.cs372.shapes

import edu.luc.cs.laufer.cs372.shapes

// File copied from "size.scala".
// Function based on copy of function "size" in "orgchart.sc" in "misc-explorations-scala".

object depth {
  def apply(s: Shape): Int = s match {
    case Rectangle(_, _) => 1
    case Ellipse(_, _) => 1
    case Location(_, _, shape @ _) => depth(shape) + 1
    case Group(shapes @ _*) => shapes.map(depth(_)).max + 1
    case _ => {println("Function \"size\" received object of unknown type."); 0}
  }
}
