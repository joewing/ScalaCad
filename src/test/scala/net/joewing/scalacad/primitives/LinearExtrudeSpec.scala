package net.joewing.scalacad.primitives

import org.scalatest.{FunSpec, Matchers}

class LinearExtrudeSpec extends FunSpec with Matchers {
  describe("extent") {
    it("makes an object of the right length") {
      val obj = LinearExtrude(Triangle(1, 2), 3)
      obj.extent.z shouldBe 3.0
    }
  }
}
