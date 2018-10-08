package net.joewing.scalacad

import org.scalatest.{FunSpec, Matchers}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

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
      val bsp = Await.result(BSPTree(Seq(polygon)), Duration.Inf).asInstanceOf[BSPTreeNode]

      bsp.plane shouldBe Plane(Vertex(0, 0, -1), 0.0)
      bsp.allPolygons shouldBe Seq(polygon)
    }
  }

  describe("allPolygons") {
    it("returns all polygons in the tree") {
      val polygon = PlanePolygon.fromVertices(Seq(Vertex(1, 0, 0), Vertex(0, 1, 0), Vertex(1, 1, 0)))
      val bsp = Await.result(BSPTree(Seq(polygon)), Duration.Inf)
      bsp.allPolygons shouldBe Seq(polygon)
    }
  }

  describe("clip") {
    val bsp = Await.result(BSPTree(obj), Duration.Inf)

    ignore("returns itself when clipped with itself") {
      Await.result(bsp.clip(bsp), Duration.Inf) shouldBe bsp
    }

    it("returns nothing when clipped with itself inverted") {
      Await.result(bsp.clip(bsp.inverted), Duration.Inf).allPolygons shouldBe Seq.empty
    }

    ignore("returns itself inverted when the inverted is clipped with the inverted") {
      bsp.inverted.clip(bsp.inverted) shouldBe bsp.inverted
    }
  }

  describe("inverted") {
    it("inverts the space") {
      val polygon = PlanePolygon.fromVertices(Seq(Vertex(1, 0, 0), Vertex(0, 1, 0), Vertex(1, 1, 0)))
      val bsp = Await.result(BSPTree(Seq(polygon)), Duration.Inf).inverted.asInstanceOf[BSPTreeNode]

      bsp.plane shouldBe Plane(Vertex(0, 0, 1), 0.0)
      bsp.polygons shouldBe Seq(polygon.flip)
    }

    it("does nothing when performed twice") {
      val bsp = Await.result(BSPTree(obj), Duration.Inf)
      bsp.inverted.inverted shouldBe bsp
    }
  }
}
