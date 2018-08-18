package net.joewing.scalacad

import org.scalatest.{FunSpec, Matchers}

import scala.collection.mutable.ArrayBuffer

class BSPTreeSpec extends FunSpec with Matchers {

  val obj = Seq(
    Polygon3d(Seq(Vertex(-0.5, 1, 0), Vertex(1, 0, 0), Vertex(0, 0, 1))),
    Polygon3d(Seq(Vertex(-0.5, -1, 0), Vertex(-0.5, 1, 0), Vertex(0, 0, 1))),
    Polygon3d(Seq(Vertex(1, 0, 0), Vertex(-0.5, -1, 0), Vertex(0, 0, 1))),
    Polygon3d(Seq(Vertex(1, 0, 0), Vertex(-0.5, 1, 0), Vertex(-0.5, -1, 0)))
  )

  describe("apply") {
    it("creates a BSP from a single polygon") {
      val polygon = Polygon3d(Seq(Vertex(1, 0, 0), Vertex(0, 1, 0), Vertex(1, 1, 0)))
      val bsp = BSPTree(Seq(polygon)).asInstanceOf[BSPTreeNode]

      bsp.plane shouldBe Plane(Vertex(0, 0, -1), 0.0)
      bsp.allPolygons shouldBe Seq(polygon)
    }
  }

  describe("allPolygons") {
    it("returns all polygons in the tree") {
      val polygon = Polygon3d(Seq(Vertex(1, 0, 0), Vertex(0, 1, 0), Vertex(1, 1, 0)))
      val bsp = BSPTree(Seq(polygon))
      bsp.allPolygons shouldBe Seq(polygon)
    }
  }

  describe("clipPolygons") {

    val bsp = BSPTree(obj)

    it("removes polygons that are not contained in this BSPTree") {
      val outside = Polygon3d(Seq(Vertex(2, 0, 0), Vertex(0, 2, 0), Vertex(2, 2, 0)))
      val inside = Polygon3d(Seq(Vertex(0.25, 0.25, 0.1), Vertex(0, 0.25, 0.2), Vertex(0.25, 0.25, 0.3)))
      bsp.clipPolygons(Seq(inside, outside)) shouldBe Seq(inside)
    }

    it("splits polygons that span the object") {
      val spanning = Polygon3d(Seq(Vertex(0, 0, 0.5), Vertex(0, 1, 0.5), Vertex(1, 1, 0.5)))
      val result = bsp.clipPolygons(Seq(spanning))
      result.length shouldBe 1

      result.head.normal shouldBe spanning.normal
      result.head.vertices(0) shouldBe Vertex(0, 0, 0.5)

      result.head.vertices(1).x shouldBe 0.0
      result.head.vertices(1).y shouldBe 0.3 +- 0.1
      result.head.vertices(1).z shouldBe 0.5

      result.head.vertices(2).x shouldBe 0.2 +- 0.1
      result.head.vertices(2).y shouldBe 0.2 +- 0.1
      result.head.vertices(2).z shouldBe 0.5
    }

    it("splits polygons that span the object preserving the orientation") {
      val spanning = Polygon3d(Seq(Vertex(0, 0, 0.5), Vertex(0, 1, 0.5), Vertex(1, 1, 0.5)))

      bsp.clipPolygons(Seq(spanning)).head.normal shouldBe spanning.normal
      bsp.clipPolygons(Seq(spanning.flip)).head.normal shouldBe spanning.flip.normal
    }

    it("the inverted bsp splits polygons that span the object") {
      val spanning = Polygon3d(Seq(Vertex(0, 0, 0.5), Vertex(0, 1, 0.5), Vertex(1, 1, 0.5)))
      val result = bsp.inverted.clipPolygons(Seq(spanning))
      result.length shouldBe 1

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
      bsp.clip(bsp.inverted).allPolygons shouldBe Seq.empty
    }

    it("returns itself inverted when the inverted is clipped with the inverted") {
      bsp.inverted.clip(bsp.inverted) shouldBe bsp.inverted
    }
  }

  describe("inverted") {
    it("inverts the space") {
      val polygon = Polygon3d(Seq(Vertex(1, 0, 0), Vertex(0, 1, 0), Vertex(1, 1, 0)))
      val bsp = BSPTree(Seq(polygon)).inverted.asInstanceOf[BSPTreeNode]

      bsp.plane shouldBe Plane(Vertex(0, 0, 1), 0.0)
      bsp.polygons shouldBe Seq(polygon.flip)
    }

    it("does nothing when performed twice") {
      val bsp = BSPTree(obj)
      bsp.inverted.inverted shouldBe bsp
    }
  }
}
