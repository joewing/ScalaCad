package net.joewing.scalacad.primitives

import org.scalatest.{FunSpec, Matchers}

class IntersectionSpec extends FunSpec with Matchers {
  describe("render") {
    val obj = Cube(1, 1, 1)

    it("should return itself when intersected with itself") {
      Intersection(obj, obj).render.facets.size shouldBe obj.render.facets.size
    }

    it("should return nothing when non-overlapping parts are intersected") {
      Intersection(obj, Translate(obj, 10)).render.facets.size shouldBe 0
    }
  }
}
