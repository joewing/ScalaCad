package net.joewing.csg.primitives

import org.scalatest.{FunSpec, Matchers}

class DifferenceSpec extends FunSpec with Matchers {
  describe("render") {
    val obj = Cylinder(10, 2, 2, 3)

    it("should return nothing when subtracted from itself") {
      Difference(obj, obj).render.allPolygons shouldBe Seq.empty
    }

    it("should return itself when subtracted from its inverse") {
      Difference(obj, Invert(obj)).render shouldBe obj.render
    }
  }
}
