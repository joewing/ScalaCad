package net.joewing.scalacad.primitives

import org.scalatest.{FunSpec, Matchers}

class RectangleSpec extends FunSpec with Matchers {
  describe("render") {
    it("has the right normals") {
      val facets = Rectangle(1, 2).rendered.facets
      facets.foreach { facet =>
        facet.normal.z should be < 0.0
      }
    }
  }
}
