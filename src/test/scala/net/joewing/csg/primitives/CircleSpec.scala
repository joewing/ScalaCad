package net.joewing.csg.primitives

import net.joewing.csg.Vertex
import org.scalatest.{FunSpec, Matchers}

class CircleSpec extends FunSpec with Matchers {
  describe("bounds") {
    it("returns the right extent") {
      Circle(4, 8).extent shouldBe Vertex(8, 8, 0)
    }

    it("returns the right min bound") {
      Circle(4, 8).minBound shouldBe Vertex(-4, -4, 0)
    }

    it("returns the right max bound") {
      Circle(4, 8).maxBound shouldBe Vertex(4, 4, 0)
    }
  }

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