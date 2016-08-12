package edu.luc.cs.laufer.cs372.shapes

import org.scalatest.FunSuite

import TestFixtures._

// Copied file from "TestBoundingBox.scala".

class TestSize extends FunSuite {

  def testSize(description: String, s: Shape, objective: Int) = {
    test(description) {
      val result = size(s)
      assert(objective === result)
    }
  }

  testSize("simple ellipse", simpleEllipse, 1)
  testSize("simple rectangle", simpleRectangle, 1)
  testSize("simple location", simpleLocation, 1)
  testSize("basic group", basicGroup, 2)
  testSize("simple group", simpleGroup, 2)
  testSize("complex group", complexGroup, 5)
}
