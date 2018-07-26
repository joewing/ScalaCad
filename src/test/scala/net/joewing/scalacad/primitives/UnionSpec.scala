package net.joewing.scalacad.primitives

import net.joewing.scalacad.{Facet, Vertex}
import org.scalatest.{FunSpec, Matchers}

class UnionSpec extends FunSpec with Matchers {
  describe("render") {
    val obj = Cylinder(10, 3, 3, 3)

    it("should completely remove contained objects") {
      val o1 = Cube(10, 10, 10)
      val o2 = Translate(Cube(1, 1, 1), 1, 1, 1)
      Union(o1, o2).render.toSet shouldBe o1.render.toSet
    }
  }
}
