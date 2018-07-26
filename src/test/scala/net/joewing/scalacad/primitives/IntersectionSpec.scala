package net.joewing.scalacad.primitives

import org.scalatest.{FunSpec, Matchers}

class IntersectionSpec extends FunSpec with Matchers {
  describe("render") {
    val obj = Cube(1, 1, 1)

    it("should return itself when intersected with itself") {
      Intersection(obj, obj).render.size shouldBe obj.render.size
    }
  }
}
