package net.joewing.scalacad.primitives

import org.scalatest.{FunSpec, Matchers}

class LinearExtrudeSpec extends FunSpec with Matchers {
  describe("extent") {
    it("makes an object of the right length") {
      val obj = LinearExtrude(Triangle(1, 2), 3)
      obj.extent.x3 shouldBe 3.0
    }
  }

  describe("render") {
    it("creates the right number of facets for a triangle with 1 slice") {
      val facets = LinearExtrude(Triangle(1, 1), 1).render
      facets.size shouldBe (1 + 3 + 1)
    }

    it("creates the right number of facets for a square with 1 slice") {
      val facets = LinearExtrude(Rectangle(1, 1), 1).render
      facets.size shouldBe (1 + 4 + 1)
    }

    it("creates the right number of facets for a triangle with 2 slices") {
      val facets = LinearExtrude(Triangle(1, 1), 1, slices = 2).render
      facets.size shouldBe (1 + 3 + 3 + 1)
    }
  }
}
