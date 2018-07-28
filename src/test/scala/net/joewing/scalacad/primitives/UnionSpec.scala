package net.joewing.scalacad.primitives

import net.joewing.scalacad.Utils
import org.scalatest.{FunSpec, Matchers}

class UnionSpec extends FunSpec with Matchers {
  describe("render") {
    it("should completely remove contained objects") {
      val o1 = Cube(10, 10, 10)
      val o2 = Translate(Cube(1, 1, 1), 1, 1, 1)
      Union(o1, o2).render.toSet shouldBe o1.render.toSet
    }

    it("should not leave unmatched edges when there is complete containment") {
      val o1 = Cube(10, 10, 10).centered
      val o2 = Cube(5, 5, 5).centered
      Utils.validateEdges(Union(o1, o2).render)
    }

    it("should not leave unmatched edges when there is overlap") {
      val o1 = Cube(10, 10, 10).centered
      val o2 = Cube(20, 5, 5).centered
      Utils.validateEdges(Union(o1, o2).render)
    }

    it("when an object is unioned with itself") {
      val obj = Cube(10, 10, 10).centered
      Utils.validateEdges(Union(obj, obj).render)
    }
  }
}
