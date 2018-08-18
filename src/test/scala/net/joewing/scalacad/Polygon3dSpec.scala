package net.joewing.scalacad

import org.scalatest.{FunSpec, FunSuite, Matchers}

class Polygon3dSpec extends FunSpec with Matchers {
  describe("minBound") {
    it("returns the min") {
      val p = Polygon3d(Seq(Vertex(2, 4, 8), Vertex(7, 3, 1), Vertex(3, 3, 3)))
      p.minBound shouldBe Vertex(2, 3, 1)
    }
  }

  describe("maxBound") {
    it("returns the max") {
      val p = Polygon3d(Seq(Vertex(2, 4, 8), Vertex(7, 3, 1), Vertex(3, 3, 3)))
      p.maxBound shouldBe Vertex(7, 4, 8)
    }
  }

  describe("edges") {
    it("returns all edges") {
      val p = Polygon3d(Seq(Vertex(2, 4, 8), Vertex(7, 3, 1), Vertex(3, 3, 3)))
      p.edges shouldBe Seq(
        (Vertex(2, 4, 8), Vertex(7, 3, 1)),
        (Vertex(7, 3, 1), Vertex(3, 3, 3)),
        (Vertex(3, 3, 3), Vertex(2, 4, 8))
      )
    }
  }
}
