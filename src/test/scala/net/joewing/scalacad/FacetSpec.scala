package net.joewing.scalacad

import org.scalatest.{FunSpec, Matchers}

class FacetSpec extends FunSpec with Matchers {
  describe("normal") {
    it("sets the normal correctly") {
      Facet(Vertex(0, 0, 0), Vertex(0, 1, 0), Vertex(1, 1, 0)).normal shouldBe Vertex(0, 0, -1)
    }
  }

  describe("coplanar") {
    it("identifies points that are coplanar") {
      Facet(Vertex(0, 0, 0), Vertex(0, 1, 0), Vertex(1, 1, 0)).coplanar(Vertex(2, 3, 0)) shouldBe true
    }

    it("identifies points that are not coplanar") {
      Facet(Vertex(0, 0, 0), Vertex(0, 1, 0), Vertex(1, 1, 0)).coplanar(Vertex(2, 3, 1)) shouldBe false
    }

    it("identifies facets that are not coplanar") {
      val facet1 = Facet(Vertex(1, 2, 3), Vertex(1, 5, 3), Vertex(2, 3, 3))
      val facet2 = Facet(Vertex(1, 1, 5), Vertex(1, 5, 7), Vertex(5, 5, 6))
      facet1.coplanar(facet2, Vertex.epsilon) shouldBe false
    }

    it("identifies facets that are parallel, but not coplanar") {
      val facet1 = Facet(Vertex(1, 2, 3), Vertex(1, 5, 3), Vertex(2, 3, 3))
      val facet2 = Facet(Vertex(1, 2, 4), Vertex(1, 5, 4), Vertex(2, 3, 4))
      facet1.coplanar(facet2, Vertex.epsilon) shouldBe false
    }

    it("identifies facets that are coplanar") {
      val facet1 = Facet(Vertex(1, 2, 3), Vertex(1, 5, 3), Vertex(2, 3, 3))
      val facet2 = Facet(Vertex(1, 1, 3), Vertex(1, 5, 3), Vertex(5, 5, 3))
      facet1.coplanar(facet2, Vertex.epsilon) shouldBe true
    }
  }

  describe("contains") {
    val facet = Facet(Vertex(0, 0, 0), Vertex(0, 1, 0), Vertex(1, 1, 0))

    it("returns true if the point is inside the facet") {
      facet.contains(Vertex(0.25, 0.25, 0)) shouldBe true
    }

    it("returns false if the point is outside of the facet") {
      facet.contains(Vertex(0.25, 0.25, 1)) shouldBe false
      facet.contains(Vertex(2, 0.25, 0)) shouldBe false
    }

    it("returns false if a coplanar point is outside of the facet") {
      val f = Facet(Vertex(1, 1, 3), Vertex(1, 5, 3), Vertex(5, 5, 3))
      f.contains(Vertex(7, 7, 3)) shouldBe false
      f.contains(Vertex(7, 5, 3)) shouldBe false
    }

    it("returns true if the point is on a vertex of the facet") {
      facet.contains(Vertex(0, 1, 0)) shouldBe true
    }

    it("returns true if the point is on an edge of the facet") {
      facet.contains(Vertex(0, 0.5, 0)) shouldBe true
    }

    it("works for some specific cases") {
      val f = Facet(Vertex(-5, -5, -5), Vertex(-5, 5, -5), Vertex(5, 5, -5))
      f.contains(Vertex(2.1213, 2.1213, -5.0000)) shouldBe true
      f.contains(Vertex(2.7716, 1.1481, -5.0000)) shouldBe false
    }
  }

  describe("onEdge") {
    val facet = Facet(Vertex(0, 0, 0), Vertex(5, 0, 0), Vertex(5, 5, 0))
    it("returns true for a point on an edge") {
      facet.onEdge(Vertex(3, 0, 0)) shouldBe true
    }

    it("returns true for a point on a vertex") {
      facet.onEdge(Vertex(5, 0, 0)) shouldBe true
    }

    it("returns false for a point inside") {
      facet.onEdge(Vertex(1, 2, 0)) shouldBe false
    }
  }
}
