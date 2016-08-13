package edu.luc.cs.laufer.cs372.shapes

import org.scalatest.FunSuite

import TestFixtures._

// Copied file from "TestSize.scala".


class TestDepth extends FunSuite {

  def testDepth(description: String, s: Shape, objective: Int) = {
    test(description) {
      val result = depth(s)
      assert(objective === result)
    }
  }

  testDepth("simple ellipse", simpleEllipse, 1)
  testDepth("simple rectangle", simpleRectangle, 1)
  testDepth("simple location", simpleLocation, 2)
  testDepth("basic group", basicGroup, 2)
  testDepth("simple group", simpleGroup, 3)
  testDepth("complex group", complexGroup, 6)
}
