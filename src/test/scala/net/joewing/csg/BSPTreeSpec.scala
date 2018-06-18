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
      val bsp = BSPTree(Seq(facet))

      bsp.plane shouldBe Plane(Vertex(0, 0, -1), 0.0)
      bsp.facets shouldBe Seq(facet)
    }
  }

  describe("allFacets") {
    it("returns all facets in the tree") {
      val facet = Facet(Vertex(1, 0, 0), Vertex(0, 1, 0), Vertex(1, 1, 0))
      val bsp = BSPTree(Seq(facet))
      bsp.allFacets shouldBe Seq(facet)
    }
  }

  describe("clipFacets") {

    val bsp = BSPTree(obj)

    it("removes facets that are not contained in this BSPTree") {
      val outside = Facet(Vertex(2, 0, 0), Vertex(0, 2, 0), Vertex(2, 2, 0))
      val inside = Facet(Vertex(0.25, 0.25, 0.1), Vertex(0, 0.25, 0.2), Vertex(0.25, 0.25, 0.3))
      bsp.clipFacets(Seq(inside, outside)) shouldBe Seq(inside)
    }

    it("splits facets that span the object") {
      val spanning = Facet(Vertex(0, 0, 0.5), Vertex(0, 1, 0.5), Vertex(1, 1, 0.5))
      val result = bsp.clipFacets(Seq(spanning))
      result.length shouldBe 1

      result.head.normal shouldBe spanning.normal
      result.head.v1 shouldBe Vertex(0, 0, 0.5)

      result.head.v2.x1 shouldBe 0.0
      result.head.v2.x2 shouldBe 0.3 +- 0.1
      result.head.v2.x3 shouldBe 0.5

      result.head.v3.x1 shouldBe 0.2 +- 0.1
      result.head.v3.x2 shouldBe 0.2 +- 0.1
      result.head.v3.x3 shouldBe 0.5
    }

    it("splits facets that span the object preserving the orientation") {
      val spanning = Facet(Vertex(0, 0, 0.5), Vertex(0, 1, 0.5), Vertex(1, 1, 0.5))

      bsp.clipFacets(Seq(spanning)).head.normal shouldBe spanning.normal
      bsp.clipFacets(Seq(spanning.flip)).head.normal shouldBe spanning.flip.normal
    }

    it("the inverted bsp splits facets that span the object") {
      val spanning = Facet(Vertex(0, 0, 0.5), Vertex(0, 1, 0.5), Vertex(1, 1, 0.5))
      val result = bsp.inverted.clipFacets(Seq(spanning))
      result.length shouldBe 2

      result.foreach { f =>
        f.normal shouldBe spanning.normal
      }
    }
  }

  describe("clip") {
    val bsp = BSPTree(obj)

    it("returns itself when clipped with itself") {
      bsp.clip(bsp) shouldBe bsp
    }

    it("returns nothing when clipped with itself inverted") {
      bsp.clip(bsp.inverted).allFacets shouldBe Seq.empty
    }

    it("returns itself inverted when the inverted is clipped with the inverted") {
      bsp.inverted.clip(bsp.inverted) shouldBe bsp.inverted
    }
  }

  describe("inverted") {
    it("inverts the space") {
      val facet = Facet(Vertex(1, 0, 0), Vertex(0, 1, 0), Vertex(1, 1, 0))
      val bsp = BSPTree(Seq(facet)).inverted

      bsp.plane shouldBe Plane(Vertex(0, 0, 1), 0.0)
      bsp.facets shouldBe Seq(facet.flip)
    }

    it("does nothing when performed twice") {
      val bsp = BSPTree(obj)
      bsp.inverted.inverted shouldBe bsp
    }
  }
}
