package net.joewing.scalacad

import org.scalatest.{FunSpec, Matchers}

import scala.collection.mutable.ArrayBuffer

class BSPTreeSpec extends FunSpec with Matchers {

  val obj = Seq(
    PlanePolygon.fromVertices(Seq(Vertex(-0.5, 1, 0), Vertex(1, 0, 0), Vertex(0, 0, 1))),
    PlanePolygon.fromVertices(Seq(Vertex(-0.5, -1, 0), Vertex(-0.5, 1, 0), Vertex(0, 0, 1))),
    PlanePolygon.fromVertices(Seq(Vertex(1, 0, 0), Vertex(-0.5, -1, 0), Vertex(0, 0, 1))),
    PlanePolygon.fromVertices(Seq(Vertex(1, 0, 0), Vertex(-0.5, 1, 0), Vertex(-0.5, -1, 0)))
  )

  describe("apply") {
    it("creates a BSP from a single polygon") {
      val polygon = PlanePolygon.fromVertices(Seq(Vertex(1, 0, 0), Vertex(0, 1, 0), Vertex(1, 1, 0)))
      val bsp = BSPTree(Seq(polygon)).asInstanceOf[BSPTreeNode]

      bsp.plane shouldBe Plane(Vertex(0, 0, -1), 0.0)
      bsp.allPolygons shouldBe Seq(polygon)
    }
  }

  describe("allPolygons") {
    it("returns all polygons in the tree") {
      val polygon = PlanePolygon.fromVertices(Seq(Vertex(1, 0, 0), Vertex(0, 1, 0), Vertex(1, 1, 0)))
      val bsp = BSPTree(Seq(polygon))
      bsp.allPolygons shouldBe Seq(polygon)
    }
  }

  describe("clip") {
    val bsp = BSPTree(obj)

    ignore("returns itself when clipped with itself") {
      bsp.clip(bsp) shouldBe bsp
    }

    it("returns nothing when clipped with itself inverted") {
      bsp.clip(bsp.inverted).allPolygons shouldBe Seq.empty
    }

    ignore("returns itself inverted when the inverted is clipped with the inverted") {
      bsp.inverted.clip(bsp.inverted) shouldBe bsp.inverted
    }
  }

  describe("inverted") {
    it("inverts the space") {
      val polygon = PlanePolygon.fromVertices(Seq(Vertex(1, 0, 0), Vertex(0, 1, 0), Vertex(1, 1, 0)))
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
