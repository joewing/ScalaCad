package net.joewing.scalacad.primitives

import org.scalatest.{FunSpec, Matchers}

class UnionSpec extends FunSpec with Matchers {
  describe("render") {
    val obj = Cylinder(10, 3, 3, 3)

    it("should return itself when unioned with itself") {
      Union(obj, obj).render shouldBe obj.render
    }
  }
}
