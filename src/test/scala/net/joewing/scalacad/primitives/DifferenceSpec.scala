package net.joewing.scalacad.primitives

import org.scalatest.{FunSpec, Matchers}

class DifferenceSpec extends FunSpec with Matchers {
  describe("render") {
    val obj = Cylinder(10, 2, 2, 3)

    it("should return nothing when subtracted from itself") {
      Difference(obj, obj).render shouldBe Seq.empty
    }
  }
}
