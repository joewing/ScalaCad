package net.joewing.scalacad.primitives

import org.scalatest.{FunSpec, Matchers}

class DifferenceSpec extends FunSpec with Matchers {
  describe("3d") {
    val obj = Cylinder(10, 2, 2, 3)

    it("should return nothing when subtracted from itself") {
      Difference(obj, obj).rendered.facets shouldBe Seq.empty
    }

    it("should return the original object when subtracted from a non-overlapping part") {
      Difference(obj, Translate(obj, 20)).rendered.facets.size shouldBe obj.rendered.facets.size
    }
  }

  describe("2d") {
    val obj = square(5)

    it("should return nothing when subtracted from itself") {
      Difference(obj, obj).rendered.facets shouldBe Seq.empty
    }

    it("should return the original object when subtracted from a non-overlapping part") {
      Difference(obj, Translate(obj, 20)).rendered.facets.size shouldBe obj.rendered.facets.size
    }
  }
}
