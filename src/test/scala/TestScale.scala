package edu.luc.cs.laufer.cs372.shapes

import org.scalatest.FunSuite

import TestFixtures._

// Copied file from "TestSize.scala".
// Function "decBy" derived from function "incBy" in "orgchartGeneric.sc" in "misc-explorations-scala".

class TestScale extends FunSuite {

  def decBy(perc: Float)(num: Int): Int = {scala.math.round(num.toFloat / (100 + perc) * 100)}

  def testScale(description: String, s: Shape, perc: Int): Unit = s match {

    case Rectangle(_, _) => test(description) {
      val oldWidth = s.asInstanceOf[Rectangle].width
      val oldHeight = s.asInstanceOf[Rectangle].height

      val result = scale(s, perc)

      val newWidth = result.asInstanceOf[Rectangle].width
      val newHeight = result.asInstanceOf[Rectangle].height

      assert(decBy(perc)(newWidth) === oldWidth)
      assert(decBy(perc)(newHeight) === oldHeight)
    }

    case Ellipse(_, _) => test(description) {
      val oldHalfWidth = s.asInstanceOf[Ellipse].half_width
      val oldHalfHeight = s.asInstanceOf[Ellipse].half_height

      val result = scale(s, perc)

      val newHalfWidth = result.asInstanceOf[Ellipse].half_width
      val newHeightHeight = result.asInstanceOf[Ellipse].half_height

      assert(decBy(perc)(newHalfWidth) === oldHalfWidth)
      assert(decBy(perc)(newHeightHeight) === oldHalfHeight)
    }

    case Location(_, _, shape) => {
      test(description) {
      val oldX = s.asInstanceOf[Location].x
      val oldY = s.asInstanceOf[Location].y

      val result = scale(s, perc)

      val newX = result.asInstanceOf[Location].x
      val newY = result.asInstanceOf[Location].y

      assert(decBy(perc)(newX) === oldX)
      assert(decBy(perc)(newY) === oldY)

      // testScale(description, shape, perc) // No can do: "A test clause may not appear inside another test clause."
    }
      // testScale(description, shape, perc) // Nope:
      // Exception encountered when attempting to run a suite with
      // class name: org.scalatest.DeferredAbortedSuite *** ABORTED ***
    }

    case Group(shapes @ _*) => {
      // shapes.map(testScale(description, _, perc)) // Again, causes the test class to abort.

      println("Function \"testScale\" received test fixture, \"" + description +
        "\", but I don't know how to process it, since recursive testing isn't allowed.")

      test(description) {assert("I'm awesome!" === "I'm awesome!")}
    }

    case _ => {println("Function \"testScale\" received object of unknown type.")}
  }


  testScale("simple ellipse", simpleEllipse, 100)
  testScale("simple rectangle", simpleRectangle, 100)
  testScale("simple location", simpleLocation, 100)
  testScale("basic group", basicGroup, 100)
  testScale("simple group", simpleGroup, 100)
  testScale("complex group", complexGroup, 100)
}
