package net.joewing.scalacad.primitives

import org.scalatest.{FunSpec, Matchers}

class TriangleSpec extends FunSpec with Matchers {
  describe("render") {
    it("should create a single facet") {
      Triangle(4, 5).rendered.facets.size shouldBe 1
    }

    it("should have a downward normal") {
      Triangle(4, 5).rendered.facets.head.normal.z should be < 0.0
    }
  }
}
