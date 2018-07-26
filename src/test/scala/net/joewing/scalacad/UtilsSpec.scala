package net.joewing.scalacad

import net.joewing.scalacad.primitives.{Cube, Sphere}
import org.scalatest.{FunSpec, Matchers}

class UtilsSpec extends FunSpec with Matchers {
  describe("edgeIntersection") {
    it("should return nothing if the segments are parallel") {
      val e1 = Vertex(1, 1, 0) -> Vertex(2, 2, 0)
      val e2 = Vertex(0, 0, 1) -> Vertex(2, 2, 1)
      Utils.edgeIntersection(e1, e2) shouldBe None
    }

    it("should return nothing if the point of intersection is off the segment") {
      val e1 = Vertex(2, 1, 0) -> Vertex(1, 2, 0)
      val e2 = Vertex(0, 0, 0) -> Vertex(1, 1, 0)
      Utils.edgeIntersection(e1, e2) shouldBe None
    }

    it("should return the point if there is an intersection xy + 1") {
      val e1 = Vertex(2, 1, 0) -> Vertex(1, 2, 0)
      val e2 = Vertex(1, 1, 0) -> Vertex(2, 2, 0)
      Utils.edgeIntersection(e1, e2) shouldBe Some(Vertex(1.5, 1.5, 0))
    }

    it("should return the point if there is an intersection xy") {
      val e1 = Vertex(1, 0, 0) -> Vertex(0, 1, 0)
      val e2 = Vertex(0, 0, 0) -> Vertex(1, 1, 0)
      Utils.edgeIntersection(e1, e2) shouldBe Some(Vertex(0.5, 0.5, 0))
    }

    it("should return the point if there is an intersection yz") {
      val e1 = Vertex(0, 0, 1) -> Vertex(0, 1, 0)
      val e2 = Vertex(0, 0, 0) -> Vertex(0, 1, 1)
      Utils.edgeIntersection(e1, e2) shouldBe Some(Vertex(0.0, 0.5, 0.5))
    }
  }

  describe("skewIntersection") {
    it("should return nothing when there is no intersection") {
      val edge = Vertex(1, 2, 3) -> Vertex(1, 5, 3)
      val facet = Facet(Vertex(1, 1, 5), Vertex(1, 5, 7), Vertex(5, 5, 6))
      Utils.skewIntersection(edge, facet) shouldBe None
    }

    it("should return nothing if the intersection is beyond the segment") {
      val edge = Vertex(2, 2, 0) -> Vertex(2, 2, 1)
      val facet = Facet(Vertex(0, 0, 2), Vertex(0, 5, 2), Vertex(5, 5, 2))
      Utils.skewIntersection(edge, facet) shouldBe None
    }

    it("should return nothing if the intersection is outside the facet") {
      val edge = Vertex(6, 6, 0) -> Vertex(6, 6, 3)
      val facet = Facet(Vertex(0, 0, 2), Vertex(0, 5, 2), Vertex(5, 5, 2))
      Utils.skewIntersection(edge, facet) shouldBe None
    }

    it("should return a point when there is an intersection") {
      val edge = Vertex(2, 2, 0) -> Vertex(2, 2, 3)
      val facet = Facet(Vertex(0, 0, 2), Vertex(0, 5, 2), Vertex(5, 5, 2))
      Utils.skewIntersection(edge, facet) shouldBe Some(Vertex(2, 2, 2))
    }

    it("should return a point when there is an intersection on an edge") {
      val edge = Vertex(2, 5, 0) -> Vertex(2, 5, 3)
      val facet = Facet(Vertex(0, 0, 2), Vertex(0, 5, 2), Vertex(5, 5, 2))
      Utils.skewIntersection(edge, facet) shouldBe Some(Vertex(2, 5, 2))
    }

    it("should return a point when there is an intersection on a vertex") {
      val edge = Vertex(0, 5, 0) -> Vertex(0, 5, 3)
      val facet = Facet(Vertex(0, 0, 2), Vertex(0, 5, 2), Vertex(5, 5, 2))
      Utils.skewIntersection(edge, facet) shouldBe Some(Vertex(0, 5, 2))
    }
  }

  describe("coplanarIntersection") {
    it("returns nothing when there is no intersection") {
      val edge = Vertex(0, 0, 0) -> Vertex(2, 2, 0)
      val facet = Facet(Vertex(3, 3, 0), Vertex(5, 3, 0), Vertex(5, 5, 0))
      Utils.coplanarIntersectionEdges(edge, facet) shouldBe None
    }

    it("returns an edge when it is completely contained") {
      val edge = Vertex(1, 1, 0) -> Vertex(2, 2, 0)
      val facet = Facet(Vertex(0, 0, 0), Vertex(5, 0, 0), Vertex(5, 5, 0))
      Utils.coplanarIntersectionEdges(edge, facet) shouldBe Some(edge)
    }

    it("returns an edge when it is touches the border") {
      val edge = Vertex(0, 0, 0) -> Vertex(2, 2, 0)
      val facet = Facet(Vertex(0, 0, 0), Vertex(5, 0, 0), Vertex(5, 5, 0))
      Utils.coplanarIntersectionEdges(edge, facet) shouldBe Some(edge)
    }

    it("returns an edge when it is partially contained") {
      val edge = Vertex(1, 1, 0) -> Vertex(1, 5, 0)
      val facet = Facet(Vertex(1, 0, 0), Vertex(2, 2, 0), Vertex(0, 2, 0))
      Utils.coplanarIntersectionEdges(edge, facet) shouldBe Some(Vertex(1, 1, 0) -> Vertex(1, 2, 0))
    }
  }

  describe("intersection") {
    it("should return nothing when there is no intersection (coplanar)") {
      val facet1 = Facet(Vertex(7, 7, 3), Vertex(7, 5, 3), Vertex(5, 7, 3))
      val facet2 = Facet(Vertex(1, 1, 3), Vertex(1, 5, 3), Vertex(5, 5, 3))
      Utils.intersection(facet1, facet2) shouldBe Seq.empty
    }

    it("should return an edge when the intersection just touches and is completely contained") {
      val facet1 = Facet(Vertex(2, 2, 5), Vertex(3, 4, 5), Vertex(3, 2, 0))
      val facet2 = Facet(Vertex(1, 1, 5), Vertex(1, 5, 5), Vertex(5, 5, 5))
      Utils.intersection(facet1, facet2) shouldBe Seq(Vertex(3, 4, 5) -> Vertex(2, 2, 5))
    }

    it("should return an edge when the intersection is completely contained") {
      val facet1 = Facet(Vertex(2, 1, 0), Vertex(2, 1, 2), Vertex(4, 3, 2))
      val facet2 = Facet(Vertex(0, 0, 1), Vertex(0, 4, 1), Vertex(4, 2, 1))
      Utils.intersection(facet1, facet2) shouldBe Seq(Vertex(2, 1, 1) -> Vertex(3, 2, 1))
    }

    it("should return the endpoint and side intersection when an edge is partially contained") {
      val facet1 = Facet(Vertex(1, 1, 0), Vertex(1, 1, 2), Vertex(4, 4, 2))
      val facet2 = Facet(Vertex(0, 0, 1), Vertex(0, 3, 1), Vertex(3, 0, 1))
      Utils.intersection(facet1, facet2) shouldBe Seq(Vertex(1, 1, 1) -> Vertex(1.5, 1.5, 1))
    }

    it("should return two intersection points when a coplanar line passes through the facet") {
      val facet1 = Facet(Vertex(0, 1, 5), Vertex(4, 3, 5), Vertex(0, 3, 0))
      val facet2 = Facet(Vertex(1, 1, 5), Vertex(1, 2, 5), Vertex(2, 2, 5))
      Utils.intersection(facet1, facet2) shouldBe Seq(Vertex(1, 1.5, 5) -> Vertex(2, 2, 5))
    }
  }

  describe("missingEdges") {
    val v1 = Vertex(0, 0, 0)
    val v2 = Vertex(0, 1, 0)
    val v3 = Vertex(1, 0, 0)
    val v4 = Vertex(1, 1, 0)
    val f1 = Facet(v1, v2, v4)
    val f2 = Facet(v1, v4, v3)

    it("returns missing edges") {
      val e = v2 -> v3
      Utils.missingEdges(Seq(e), Seq(f1, f2)) shouldBe Seq(e)
    }

    it("does not return contained edges") {
      val e = v4 -> v1
      Utils.missingEdges(Seq(e), Seq(f1, f2)) shouldBe Seq.empty
    }

    it("does not return contained reversed edges") {
      val e = v1 -> v4
      Utils.missingEdges(Seq(e), Seq(f1, f2)) shouldBe Seq.empty
    }
  }

  describe("flip") {
    val v1 = Vertex(0, 0, 0)
    val v2 = Vertex(0, 1, 0)
    val v3 = Vertex(1, 0, 0)
    val v4 = Vertex(1, 1, 0)

    it("should flip facets to get the required edge") {
      val f1 = Facet(v1, v2, v4)
      val f2 = Facet(v1, v4, v3)
      Utils.flip(f1, f2, v2 -> v3) shouldBe (Facet(v1, v2, v3), Facet(v4, v3, v2))
    }

    it("should flip facets to get the required edge (reversed)") {
      val f1 = Facet(v1, v4, v2)
      val f2 = Facet(v1, v3, v4)
      Utils.flip(f1, f2, v2 -> v3) shouldBe (Facet(v1, v3, v2), Facet(v4, v2, v3))
    }
  }

  describe("isContained") {
    val obj = Cube(4, 4, 4).centered.render

    it("should return true if contained") {
      Utils.isContained(obj, Vertex(1, 1, 1)) shouldBe true
      Utils.isContained(obj, Vertex(0, 0, 0)) shouldBe true
      Utils.isContained(obj, Vertex(0, 0, 1)) shouldBe true
      Utils.isContained(obj, Vertex(0, 1, 1)) shouldBe true
      Utils.isContained(obj, Vertex(1.1, 0.3, -0.2)) shouldBe true
      Utils.isContained(obj, Vertex(2, 0, 0)) shouldBe true
      Utils.isContained(obj, Vertex(2, 2, 2)) shouldBe true
      Utils.isContained(obj, Vertex(-2, -2, -2)) shouldBe true
    }

    it("should return false if not contained") {
      Utils.isContained(obj, Vertex(5, 5, 5)) shouldBe false
      Utils.isContained(obj, Vertex(0, 0, 5)) shouldBe false
      Utils.isContained(obj, Vertex(5, 5, 0)) shouldBe false
    }
  }
}
