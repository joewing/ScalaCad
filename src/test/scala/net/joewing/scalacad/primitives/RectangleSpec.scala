package net.joewing.scalacad.primitives

import org.scalatest.{FunSpec, Matchers}

class RectangleSpec extends FunSpec with Matchers {
  describe("render") {
    it("returns a valid object") {
      val bsp = Rectangle(1, 2).render
      bsp.inverted.inverted shouldBe bsp
    }

    it("has the right normals") {
      val facets = Rectangle(1, 2).render.allPolygons
      facets.foreach { facet =>
        facet.normal.x3 should be < 0.0
      }
    }
  }
}
