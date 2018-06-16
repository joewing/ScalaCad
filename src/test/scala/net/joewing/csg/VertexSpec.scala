package net.joewing.csg

import org.scalatest.{FunSpec, Matchers}

class VertexSpec extends FunSpec with Matchers {
  describe("scaled") {
    it("scales x") {
      Vertex(1, 1, 1).scaled(x = 2) shouldBe Vertex(2, 1, 1)
    }

    it("scales y") {
      Vertex(1, 1, 1).scaled(y = 2) shouldBe Vertex(1, 2, 1)
    }

    it("scales z") {
      Vertex(1, 1, 1).scaled(z = 2) shouldBe Vertex(1, 1, 2)
    }
  }

  describe("rotated") {
    it("rotates about x") {
      Vertex(1, 1, 1).rotated(x = math.Pi).x1 shouldBe 1.0 +- 1e-6
      Vertex(1, 1, 1).rotated(x = math.Pi).x2 shouldBe -1.0 +- 1e-6
      Vertex(1, 1, 1).rotated(x = math.Pi).x3 shouldBe -1.0 +- 1e-6
    }

    it("rotates about y") {
      Vertex(1, 1, 1).rotated(y = math.Pi).x1 shouldBe -1.0 +- 1e-6
      Vertex(1, 1, 1).rotated(y = math.Pi).x2 shouldBe 1.0 +- 1e-6
      Vertex(1, 1, 1).rotated(y = math.Pi).x3 shouldBe -1.0 +- 1e-6
    }

    it("rotates about z") {
      Vertex(1, 1, 1).rotated(z = math.Pi).x1 shouldBe -1.0 +- 1e-6
      Vertex(1, 1, 1).rotated(z = math.Pi).x2 shouldBe -1.0 +- 1e-6
      Vertex(1, 1, 1).rotated(z = math.Pi).x3 shouldBe 1.0 +- 1e-6
    }
  }

  describe("length") {
    it("returns the length") {
      Vertex(3, 6, 6).length shouldBe 9.0 +- 1e-6
    }
  }

  describe("negated") {
    it("negates") {
      Vertex(1, -1, 2).negated shouldBe Vertex(-1, 1, -2)
    }
  }
}
