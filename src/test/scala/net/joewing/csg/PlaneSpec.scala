package net.joewing.csg

import org.scalatest.{FunSpec, Matchers}

class PlaneSpec extends FunSpec with Matchers {
  describe("apply") {
    it("creates a plane from a facet") {
      val plane = Plane(Polygon(Seq(Vertex(0, 0, 0), Vertex(1, 0, 0), Vertex(1, 1, 0))))
      plane.normal shouldBe Vertex(0, 0, 1)
      plane.w shouldBe 0.0
    }
  }

  describe("classify") {
    it("identifies co-planar vertices") {
      val plane = Plane(Polygon(Seq(Vertex(0, 0, 0), Vertex(1, 0, 0), Vertex(1, 1, 0))))
      plane.classify(Vertex(0, 1, 0)) shouldBe Plane.Coplanar
    }

    it("identifies vertices in front of the plane") {
      val plane = Plane(Polygon(Seq(Vertex(0, 0, 0), Vertex(1, 0, 0), Vertex(1, 1, 0))))
      plane.classify(Vertex(0, 1, 1)) shouldBe Plane.Front
    }

    it("identifies vertices behind the plane") {
      val plane = Plane(Polygon(Seq(Vertex(0, 0, 0), Vertex(1, 0, 0), Vertex(1, 1, 0))))
      plane.classify(Vertex(0, 1, -1)) shouldBe Plane.Back
    }
  }

  describe("splitFacet") {
    it("splits facets behind the plane") {
      val plane = Plane(Facet(Vertex(0, 0, 0), Vertex(1, 0, 0), Vertex(1, 1, 0)))
      val f = Polygon(Seq(Vertex(0, 0, -1), Vertex(1, 0, -1), Vertex(1, 1, -1)))
      val result = Plane.SplitResult()
      plane.splitPolygon(f, result)
      result.back shouldBe Seq(f)
      result.front shouldBe Seq.empty
      result.coFront shouldBe Seq.empty
      result.coBack shouldBe Seq.empty
    }

    it("splits facets in front of the plane") {
      val plane = Plane(Facet(Vertex(0, 0, 0), Vertex(1, 0, 0), Vertex(1, 1, 0)))
      val f = Polygon(Seq(Vertex(0, 0, 1), Vertex(1, 0, 1), Vertex(1, 1, 1)))
      val result = Plane.SplitResult()
      plane.splitPolygon(f, result)
      result.back shouldBe Seq.empty
      result.front shouldBe Seq(f)
      result.coFront shouldBe Seq.empty
      result.coBack shouldBe Seq.empty
    }

    it("splits facets co-planar in front of the plane") {
      val plane = Plane(Facet(Vertex(0, 0, 0), Vertex(1, 0, 0), Vertex(1, 1, 0)))
      val f = Polygon(Seq(Vertex(0, 0, 0), Vertex(1, 0, 0), Vertex(1, 1, 0)))
      val result = Plane.SplitResult()
      plane.splitPolygon(f, result)
      result.back shouldBe Seq.empty
      result.front shouldBe Seq.empty
      result.coFront shouldBe Seq(f)
      result.coBack shouldBe Seq.empty
    }

    it("splits facets co-planar in back of the plane") {
      val plane = Plane(Facet(Vertex(0, 0, 0), Vertex(1, 0, 0), Vertex(1, 1, 0)))
      val f = Polygon(Seq(Vertex(1, 1, 0), Vertex(1, 0, 0), Vertex(0, 0, 0)))
      val result = Plane.SplitResult()
      plane.splitPolygon(f, result)
      result.back shouldBe Seq.empty
      result.front shouldBe Seq.empty
      result.coFront shouldBe Seq.empty
      result.coBack shouldBe Seq(f)
    }

    it("splits facets spanning the plane") {
      val plane = Plane(Facet(Vertex(0, 0, 0), Vertex(1, 0, 0), Vertex(1, 1, 0)))
      val f = Polygon(Seq(Vertex(0, 0, -1), Vertex(1, 0, 1), Vertex(1, 1, 1)))
      val result = Plane.SplitResult()
      plane.splitPolygon(f, result)
      result.front shouldBe Seq(
        Facet(Vertex(0.5, 0, 0), Vertex(1, 0, 1), Vertex(1, 1, 1)),
        Facet(Vertex(1, 1, 1), Vertex(0.5, 0.5, 0), Vertex(0.5, 0, 0))
      )
      result.back shouldBe Seq(Facet(Vertex(0, 0, -1), Vertex(0.5, 0, 0), Vertex(0.5, 0.5, 0)))
      result.coFront shouldBe Seq.empty
      result.coBack shouldBe Seq.empty
    }
  }

  describe("split") {
    it("combines the results from splitFacets") {
      val plane = Plane(Facet(Vertex(0, 0, 0), Vertex(1, 0, 0), Vertex(1, 1, 0)))
      val f1 = Polygon(Seq(Vertex(0, 0, -1), Vertex(1, 0, -1), Vertex(1, 1, -1)))
      val f2 = Polygon(Seq(Vertex(0, 0, 1), Vertex(1, 0, 1), Vertex(1, 1, 1)))
      val result = plane.split(Seq(f1, f2))
      result.back shouldBe Seq(f1)
      result.front shouldBe Seq(f2)
      result.coFront shouldBe Seq.empty
      result.coBack shouldBe Seq.empty
    }
  }
}
