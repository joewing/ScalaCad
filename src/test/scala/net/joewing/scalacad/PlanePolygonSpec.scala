package net.joewing.scalacad

import org.scalatest.{FunSpec, Matchers}

class PlanePolygonSpec extends FunSpec with Matchers {
  describe("to/from vertices") {
    val vertices = Seq(
      Vertex(0, 0, 0),
      Vertex(1, 0, 0),
      Vertex(1, 1, 0),
      Vertex(0, 1, 0)
    )

    it("creates the right number of planes") {
      val pp = PlanePolygon.fromVertices(vertices)
      pp.planes.size shouldBe 5
    }

    it("creates the right number of vertices") {
      PlanePolygon.fromVertices(vertices).vertices.size shouldBe 4
    }

    it("recovers the right vertices") {
      val pp = PlanePolygon.fromVertices(vertices)
      pp.vertices shouldBe vertices
    }
  }

  describe("flip") {
    it("flips the polygon") {
      val vertices = Seq(
        Vertex(0, 0, 0),
        Vertex(1, 0, 0),
        Vertex(1, 1, 0),
        Vertex(0, 1, 0)
      )
      val flipped = Seq(
        Vertex(0, 0, 0),
        Vertex(0, 1, 0),
        Vertex(1, 1, 0),
        Vertex(1, 0, 0)
      )

      val pp = PlanePolygon.fromVertices(vertices)
      pp.flip.vertices shouldBe flipped
    }

    it("returns to the original polygon when done twice") {
      val vertices = Seq(
        Vertex(0, 0, 0),
        Vertex(1, 0, 0),
        Vertex(1, 1, 0),
        Vertex(0, 1, 0)
      )

      val pp = PlanePolygon.fromVertices(vertices)
      pp.flip.flip.vertices shouldBe vertices
    }
  }
}
