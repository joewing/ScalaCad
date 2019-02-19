package net.joewing.scalacad

import org.scalatest.{FunSpec, Matchers}

class PlaneSpec extends FunSpec with Matchers {
  describe("apply") {
    it("creates a plane from a polygon") {
      val plane = Plane(Polygon3d(Vector(Vertex(0, 0, 0), Vertex(1, 0, 0), Vertex(1, 1, 0))))
      plane.normal shouldBe Vertex(0, 0, 1)
      plane.w shouldBe 0.0
    }
  }

  describe("classify") {
    it("identifies co-planar vertices") {
      val plane = Plane(Polygon3d(Vector(Vertex(0, 0, 0), Vertex(1, 0, 0), Vertex(1, 1, 0))))
      plane.classify(Vertex(0, 1, 0)) shouldBe Plane.Coplanar
    }

    it("identifies vertices in front of the plane") {
      val plane = Plane(Polygon3d(Vector(Vertex(0, 0, 0), Vertex(1, 0, 0), Vertex(1, 1, 0))))
      plane.classify(Vertex(0, 1, 1)) shouldBe Plane.Front
    }

    it("identifies vertices behind the plane") {
      val plane = Plane(Polygon3d(Vector(Vertex(0, 0, 0), Vertex(1, 0, 0), Vertex(1, 1, 0))))
      plane.classify(Vertex(0, 1, -1)) shouldBe Plane.Back
    }
  }

  describe("splitPolygon") {
    it("splits polygons behind the plane") {
      val plane = Plane(Polygon3d(Vector(Vertex(0, 0, 0), Vertex(1, 0, 0), Vertex(1, 1, 0))))
      val f = Polygon3d(Vector(Vertex(0, 0, -1), Vertex(1, 0, -1), Vertex(1, 1, -1)))
      val result = new Plane.SplitResult()
      plane.splitPolygon(f, result)
      result.back.result shouldBe Array(f)
      result.front.result shouldBe Array.empty
      result.coFront.result shouldBe Array.empty
      result.coBack.result shouldBe Array.empty
    }

    it("splits polygons in front of the plane") {
      val plane = Plane(Polygon3d(Vector(Vertex(0, 0, 0), Vertex(1, 0, 0), Vertex(1, 1, 0))))
      val f = Polygon3d(Vector(Vertex(0, 0, 1), Vertex(1, 0, 1), Vertex(1, 1, 1)))
      val result = new Plane.SplitResult()
      plane.splitPolygon(f, result)
      result.back.result shouldBe Array.empty
      result.front.result shouldBe Array(f)
      result.coFront.result shouldBe Array.empty
      result.coBack.result shouldBe Array.empty
    }

    it("splits polygons co-planar in front of the plane") {
      val plane = Plane(Polygon3d(Vector(Vertex(0, 0, 0), Vertex(1, 0, 0), Vertex(1, 1, 0))))
      val f = Polygon3d(Vector(Vertex(0, 0, 0), Vertex(1, 0, 0), Vertex(1, 1, 0)))
      val result = new Plane.SplitResult()
      plane.splitPolygon(f, result)
      result.back.result shouldBe Array.empty
      result.front.result shouldBe Array.empty
      result.coFront.result shouldBe Array(f)
      result.coBack.result shouldBe Array.empty
    }

    it("splits polygons co-planar in back of the plane") {
      val plane = Plane(Polygon3d(Vector(Vertex(0, 0, 0), Vertex(1, 0, 0), Vertex(1, 1, 0))))
      val f = Polygon3d(Vector(Vertex(1, 1, 0), Vertex(1, 0, 0), Vertex(0, 0, 0)))
      val result = new Plane.SplitResult()
      plane.splitPolygon(f, result)
      result.back.result shouldBe Array.empty
      result.front.result shouldBe Array.empty
      result.coFront.result shouldBe Array.empty
      result.coBack.result shouldBe Array(f)
    }

    it("splits polygons spanning the plane") {
      val plane = Plane(Polygon3d(Vector(Vertex(0, 0, 0), Vertex(1, 0, 0), Vertex(1, 1, 0))))
      val f = Polygon3d(Vector(Vertex(0, 0, -1), Vertex(1, 0, 1), Vertex(1, 1, 1)))
      val result = new Plane.SplitResult()
      plane.splitPolygon(f, result)
      result.front.result shouldBe Array(
        Polygon3d(Vector(Vertex(0.5, 0, 0), Vertex(1, 0, 1), Vertex(1, 1, 1), Vertex(0.5, 0.5, 0)))
      )
      result.back.result shouldBe Array(Polygon3d(Vector(Vertex(0, 0, -1), Vertex(0.5, 0, 0), Vertex(0.5, 0.5, 0))))
      result.coFront.result shouldBe Array.empty
      result.coBack.result shouldBe Array.empty
    }
  }

  describe("split") {
    it("combines the results from splitPolygons") {
      val plane = Plane(Polygon3d(Vector(Vertex(0, 0, 0), Vertex(1, 0, 0), Vertex(1, 1, 0))))
      val f1 = Polygon3d(Vector(Vertex(0, 0, -1), Vertex(1, 0, -1), Vertex(1, 1, -1)))
      val f2 = Polygon3d(Vector(Vertex(0, 0, 1), Vertex(1, 0, 1), Vertex(1, 1, 1)))
      val result = plane.split(Vector(f1, f2))
      result.back.result shouldBe Array(f1)
      result.front.result shouldBe Array(f2)
      result.coFront.result shouldBe Array.empty
      result.coBack.result shouldBe Array.empty
    }
  }
}
