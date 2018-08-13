package net.joewing.scalacad

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
      Vertex(1, 1, 1).rotated(x = math.Pi).x shouldBe 1.0 +- 1e-6
      Vertex(1, 1, 1).rotated(x = math.Pi).y shouldBe -1.0 +- 1e-6
      Vertex(1, 1, 1).rotated(x = math.Pi).z shouldBe -1.0 +- 1e-6
    }

    it("rotates about y") {
      Vertex(1, 1, 1).rotated(y = math.Pi).x shouldBe -1.0 +- 1e-6
      Vertex(1, 1, 1).rotated(y = math.Pi).y shouldBe 1.0 +- 1e-6
      Vertex(1, 1, 1).rotated(y = math.Pi).z shouldBe -1.0 +- 1e-6
    }

    it("rotates about z") {
      Vertex(1, 1, 1).rotated(z = math.Pi).x shouldBe -1.0 +- 1e-6
      Vertex(1, 1, 1).rotated(z = math.Pi).y shouldBe -1.0 +- 1e-6
      Vertex(1, 1, 1).rotated(z = math.Pi).z shouldBe 1.0 +- 1e-6
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

  describe("between") {
    it("correctly identifies a point between two others") {
      Vertex(2, 4, 6).between(Vertex(1, 2, 3), Vertex(4, 8, 12)) shouldBe true
    }

    it("correctly identifies a point beyond the second") {
      Vertex(4, 8, 12).between(Vertex(1, 2, 3), Vertex(4, 4, 6)) shouldBe false
    }

    it("correctly identifies a point before the first") {
      Vertex(0.5, 1, 1.5).between(Vertex(1, 2, 3), Vertex(4, 4, 6)) shouldBe false
    }

    it("correctly identifies a duplicate first point") {
      Vertex(1, 2, 3).between(Vertex(1, 2, 3), Vertex(4, 4, 6)) shouldBe false
    }

    it("correctly identifies a duplicate last point") {
      Vertex(4, 4, 6).between(Vertex(1, 2, 3), Vertex(4, 4, 6)) shouldBe false
    }
  }

  describe("collinear") {
    it("identifies points that are collinear") {
      Vertex(2, 4, 6).collinear(Vertex(1, 2, 3), Vertex(4, 8, 12)) shouldBe true
    }

    it("identifies points that are not collinear") {
      Vertex(2, 4, 5).collinear(Vertex(1, 2, 3), Vertex(4, 8, 12)) shouldBe false
    }
  }
}
