package net.joewing.csg

import org.scalatest.{FunSpec, Matchers}

import scala.collection.mutable.ArrayBuffer

class BSPTreeSpec extends FunSpec with Matchers {

  val obj = Seq(
    Facet(Vertex(-0.5, 1, 0), Vertex(1, 0, 0), Vertex(0, 0, 1)),
    Facet(Vertex(-0.5, -1, 0), Vertex(-0.5, 1, 0), Vertex(0, 0, 1)),
    Facet(Vertex(1, 0, 0), Vertex(-0.5, -1, 0), Vertex(0, 0, 1)),
    Facet(Vertex(1, 0, 0), Vertex(-0.5, 1, 0), Vertex(-0.5, -1, 0))
  )

  describe("apply") {
    it("creates a BSP from a single facet") {
      val facet = Facet(Vertex(1, 0, 0), Vertex(0, 1, 0), Vertex(1, 1, 0))
      val bsp = BSPTree.fromFacets(Seq(facet))

      bsp.plane shouldBe Plane(Vertex(0, 0, -1), 0.0)
      bsp.toFacets shouldBe Seq(facet)
    }
  }

  describe("allFacets") {
    it("returns all facets in the tree") {
      val facet = Facet(Vertex(1, 0, 0), Vertex(0, 1, 0), Vertex(1, 1, 0))
      val bsp = BSPTree.fromFacets(Seq(facet))
      bsp.toFacets shouldBe Seq(facet)
    }
  }

  describe("clipFacets") {

    val bsp = BSPTree.fromFacets(obj)

    it("removes facets that are not contained in this BSPTree") {
      val outside = Polygon(Seq(Vertex(2, 0, 0), Vertex(0, 2, 0), Vertex(2, 2, 0)))
      val inside = Polygon(Seq(Vertex(0.25, 0.25, 0.1), Vertex(0, 0.25, 0.2), Vertex(0.25, 0.25, 0.3)))
      bsp.clipPolygons(Seq(inside, outside)) shouldBe Seq(inside)
    }

    it("splits facets that span the object") {
      val spanning = Polygon(Seq(Vertex(0, 0, 0.5), Vertex(0, 1, 0.5), Vertex(1, 1, 0.5)))
      val result = bsp.clipPolygons(Seq(spanning))
      result.length shouldBe 1

      result.head.normal shouldBe spanning.normal
      result.head.vertices(0) shouldBe Vertex(0, 0, 0.5)

      result.head.vertices(1).x1 shouldBe 0.0
      result.head.vertices(1).x2 shouldBe 0.3 +- 0.1
      result.head.vertices(1).x3 shouldBe 0.5

      result.head.vertices(2).x1 shouldBe 0.2 +- 0.1
      result.head.vertices(2).x2 shouldBe 0.2 +- 0.1
      result.head.vertices(2).x3 shouldBe 0.5
    }

    it("splits facets that span the object preserving the orientation") {
      val spanning = Polygon(Seq(Vertex(0, 0, 0.5), Vertex(0, 1, 0.5), Vertex(1, 1, 0.5)))

      bsp.clipPolygons(Seq(spanning)).head.normal shouldBe spanning.normal
      bsp.clipPolygons(Seq(spanning.flip)).head.normal shouldBe spanning.flip.normal
    }

    it("the inverted bsp splits facets that span the object") {
      val spanning = Polygon(Seq(Vertex(0, 0, 0.5), Vertex(0, 1, 0.5), Vertex(1, 1, 0.5)))
      val result = bsp.inverted.clipPolygons(Seq(spanning))
      result.length shouldBe 1

      result.foreach { f =>
        f.normal shouldBe spanning.normal
      }
    }
  }

  describe("clip") {
    val bsp = BSPTree.fromFacets(obj)

    it("returns itself when clipped with itself") {
      bsp.clip(bsp) shouldBe bsp
    }

    it("returns nothing when clipped with itself inverted") {
      bsp.clip(bsp.inverted).allPolygons shouldBe Seq.empty
    }

    it("returns itself inverted when the inverted is clipped with the inverted") {
      bsp.inverted.clip(bsp.inverted) shouldBe bsp.inverted
    }
  }

  describe("inverted") {
    it("inverts the space") {
      val polygon = Polygon(Seq(Vertex(1, 0, 0), Vertex(0, 1, 0), Vertex(1, 1, 0)))
      val bsp = BSPTree(Seq(polygon)).inverted

      bsp.plane shouldBe Plane(Vertex(0, 0, 1), 0.0)
      bsp.polygons shouldBe Seq(polygon.flip)
    }

    it("does nothing when performed twice") {
      val bsp = BSPTree.fromFacets(obj)
      bsp.inverted.inverted shouldBe bsp
    }
  }
}
