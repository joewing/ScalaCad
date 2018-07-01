package net.joewing.scalacad.io

import net.joewing.scalacad.{Polygon, Vertex}
import org.scalatest.{FunSpec, Matchers}

class FacetSpec extends FunSpec with Matchers {
  describe("normal") {
    it("sets the normal correctly") {
      Facet(Vertex(0, 0, 0), Vertex(0, 1, 0), Vertex(1, 1, 0)).normal shouldBe Vertex(0, 0, -1)
    }
  }

  describe("between") {
    it("correctly identifies a point between two others") {
      Vertex(2, 4, 6).between(Vertex(1, 2, 3), Vertex(4, 8, 12)) shouldBe true
    }

    it("correctly identifies a point beyond the second") {
      Vertex(4, 8, 12).between(Vertex(1, 2, 3), Vertex(4, 4, 6)) shouldBe false
    }

    it("correctly identifies a point before the first") {
      Vertex(0.5, 1, 1.5).between(Vertex(1, 2, 3), Vertex(4, 4, 6)) shouldBe false
    }

    it("correctly identifies a duplicate first point") {
      Vertex(1, 2, 3).between(Vertex(1, 2, 3), Vertex(4, 4, 6)) shouldBe false
    }

    it("correctly identifies a duplicate last point") {
      Vertex(4, 4, 6).between(Vertex(1, 2, 3), Vertex(4, 4, 6)) shouldBe false
    }
  }

  describe("collinear") {
    it("identifies points that are collinear") {
      Vertex(2, 4, 6).collinear(Vertex(1, 2, 3), Vertex(4, 8, 12)) shouldBe true
    }

    it("identifies points that are not collinear") {
      Vertex(2, 4, 5).collinear(Vertex(1, 2, 3), Vertex(4, 8, 12)) shouldBe false
    }
  }

  describe("containedVertices") {
    it("returns vertices in order that are collinear and between for points in increasing order") {
      val a = Vertex(1, 2, 3)
      val b = Vertex(4, 8, 12)
      val before = Vertex(0.5, 1, 1.5)
      val after = Vertex(8, 16, 24)
      val nonCollinear = Vertex(2, 5, 6)
      val contained1 = Vertex(2, 4, 6)
      val contained2 = Vertex(1.5, 3.0, 4.5)
      val contained3 = Vertex(2.5, 5.0, 7.5)

      val result = Facet.containedVertices(a, b, Seq(before, after, nonCollinear, contained1, contained2, contained3))
      result shouldBe Seq(contained2, contained1, contained3)
    }

    it("returns vertices in order that are collinear and between for points in decreasing order") {
      val a = Vertex(1, 2, 3)
      val b = Vertex(4, 8, 12)
      val before = Vertex(0.5, 1, 1.5)
      val after = Vertex(8, 16, 24)
      val nonCollinear = Vertex(2, 5, 6)
      val contained1 = Vertex(2, 4, 6)
      val contained2 = Vertex(1.5, 3.0, 4.5)
      val contained3 = Vertex(2.5, 5.0, 7.5)

      val result = Facet.containedVertices(b, a, Seq(before, after, nonCollinear, contained1, contained2, contained3))
      result shouldBe Seq(contained3, contained1, contained2)
    }

    it("returns vertices in order that are collinear and between that span the origin") {
      val a = Vertex(1, 2, 3) * -1
      val b = Vertex(4, 8, 12)
      val after = Vertex(8, 16, 24)
      val contained0 = Vertex(0.5, 1, 1.5) * -1
      val nonCollinear = Vertex(2, 5, 6)
      val contained1 = Vertex(2, 4, 6)
      val contained2 = Vertex(1.5, 3.0, 4.5)
      val contained3 = Vertex(2.5, 5.0, 7.5)

      val result = Facet.containedVertices(a, b, Seq(contained0, after, nonCollinear, contained1, contained2, contained3))
      result shouldBe Seq(contained0, contained2, contained1, contained3)
    }
  }

  describe("fromPolygons") {
    it("creates a facet from a triangle") {
      val p = Polygon(Seq(Vertex(1, 2, 1), Vertex(4, 5, 6), Vertex(7, 8, 9)))
      val expected = Seq(Facet(Vertex(1, 2, 1), Vertex(4, 5, 6), Vertex(7, 8, 9)))
      Facet.fromPolygons(Seq(p)) shouldBe expected
    }

    it("creates a facet from a triangle (reversed normal)") {
      val p = Polygon(Seq(Vertex(7, 8, 9), Vertex(4, 5, 6), Vertex(1, 2, 1)))
      val expected = Seq(Facet(Vertex(7, 8, 9), Vertex(4, 5, 6), Vertex(1, 2, 1)))
      Facet.fromPolygons(Seq(p)) shouldBe expected
    }

    it("creates facets from a square") {
      val p = Polygon(Seq(Vertex(0, 0, 1), Vertex(1, 0, 1), Vertex(1, 1, 1), Vertex(0, 1, 1)))
      val expected = Seq(
        Facet(Vertex(0, 0, 1), Vertex(1, 0, 1), Vertex(1, 1, 1)),
        Facet(Vertex(0, 0, 1), Vertex(1, 1, 1), Vertex(0, 1, 1))
      )
      Facet.fromPolygons(Seq(p)) shouldBe expected
    }

    it("creates two facets from two triangles") {
      val t1 = Polygon(Seq(Vertex(0, 0, 1), Vertex(1, 0, 1), Vertex(1, 1, 1)))
      val t2 = Polygon(Seq(Vertex(0, 0, 1), Vertex(1, 1, 1), Vertex(0, 1, 1)))
      val expected = Seq(
        Facet(Vertex(0, 0, 1), Vertex(1, 0, 1), Vertex(1, 1, 1)),
        Facet(Vertex(0, 0, 1), Vertex(1, 1, 1), Vertex(0, 1, 1))
      )
      Facet.fromPolygons(Seq(t1, t2)) shouldBe expected
    }

    it("inserts extra facets for vertices on side v1-v2") {
      val t1 = Polygon(Seq(Vertex(0, 0, 1), Vertex(2, 0, 1), Vertex(2, 2, 1)))
      val t2 = Polygon(Seq(Vertex(0, 0, 1), Vertex(1, 1, 1), Vertex(0, 1, 1)))
      val expected = Seq(
        Facet(Vertex(1, 1, 1), Vertex(2, 0, 1), Vertex(2, 2, 1)),
        Facet(Vertex(0, 0, 1), Vertex(2, 0, 1), Vertex(1, 1, 1)),
        Facet(Vertex(0, 0, 1), Vertex(1, 1, 1), Vertex(0, 1, 1))
      )
      Facet.fromPolygons(Seq(t1, t2)) shouldBe expected
    }
  }
}
