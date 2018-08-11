package net.joewing.scalacad.primitives

import org.scalatest.{FunSpec, Matchers}

class UnionSpec extends FunSpec with Matchers {
  describe("render") {
    it("should render") {
      val o1 = Cube(10, 10, 10)
      val o2 = Translate(Cube(1, 1, 1), 1, 1, 1)
      Union(o1, o2).render.facets should not be empty
    }
  }
}
