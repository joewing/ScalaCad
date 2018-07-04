package net.joewing.scalacad.primitives

import org.scalatest.{FunSpec, Matchers}

class IntersectionSpec extends FunSpec with Matchers {
  describe("render") {
    val obj = Cylinder(10, 5, 5, 4)

    it("should return itself when intersected with itself") {
      Intersection(obj, obj).render shouldBe obj.render
    }

    it("should return nothing when intersected with itself inverted") {
      Intersection(obj, Invert(obj)).render shouldBe Seq.empty
    }
  }
}
