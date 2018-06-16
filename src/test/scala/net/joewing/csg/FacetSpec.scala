package net.joewing.csg

import org.scalatest.{FunSpec, Matchers}

class FacetSpec extends FunSpec with Matchers {
  describe("normal") {
    it("sets the normal correctly") {
      Facet(Vertex(0, 0, 0), Vertex(0, 1, 0), Vertex(1, 1, 0)).normal shouldBe Vertex(0, 0, -1)
    }
  }

  describe("flip") {
    it("flips the normal") {
      Facet(Vertex(0, 0, 0), Vertex(0, 1, 0), Vertex(1, 1, 0)).flip.normal shouldBe Vertex(0, 0, 1)
    }
  }

}
