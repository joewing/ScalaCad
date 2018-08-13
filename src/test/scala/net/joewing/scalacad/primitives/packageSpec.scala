package net.joewing.scalacad.primitives

import net.joewing.scalacad.Vertex
import org.scalatest.{FunSpec, Matchers}

class packageSpec extends FunSpec with Matchers {

  val obj1 = Cube(1, 1, 1).translate(1, 1, 1)
  val obj2 = Cube(1, 1, 1).translate(-1, -1, -1)

  describe("above") {
    it("updates the object bounds") {
      val result = obj1.above(obj2)
      result.minBound shouldBe Vertex(-1, -1, -1)
      result.maxBound shouldBe Vertex(2, 2, 1)
      result.extent shouldBe Vertex(3, 3, 2)
    }
  }

  describe("below") {
    it("updates the object bounds") {
      val result = obj1.below(obj2)
      result.minBound shouldBe Vertex(-1, -1, -2)
      result.maxBound shouldBe Vertex(2, 2, 0)
      result.extent shouldBe Vertex(3, 3, 2)
    }
  }

  describe("beside") {
    it("updates the object bounds") {
      val result = obj1.beside(obj2)
      result.minBound shouldBe Vertex(-1, -1, -1)
      result.maxBound shouldBe Vertex(1, 2, 2)
      result.extent shouldBe Vertex(2, 3, 3)
    }
  }

  describe("behind") {
    it("updates the object bounds") {
      val result = obj1.behind(obj2)
      result.minBound shouldBe Vertex(-1, -1, -1)
      result.maxBound shouldBe Vertex(2, 1, 2)
      result.extent shouldBe Vertex(3, 2, 3)
    }
  }
}
