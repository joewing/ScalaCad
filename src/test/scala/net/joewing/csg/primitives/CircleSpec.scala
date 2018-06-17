package net.joewing.csg.primitives

import org.scalatest.{FunSpec, Matchers}

class CircleSpec extends FunSpec with Matchers {
  describe("render") {
    it("should return the right number of facets") {
      val slices = 8
      Circle(4, slices).render.allFacets.size shouldBe slices
    }

    it("should have all normals pointing down") {
      val facets = Circle(4, 6).render.allFacets
      facets.foreach { facet =>
        facet.normal.x3 should be < 0.0
      }
    }
  }
}
