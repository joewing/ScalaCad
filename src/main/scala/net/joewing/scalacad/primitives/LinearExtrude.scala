package net.joewing.scalacad.primitives

import net.joewing.scalacad.{Facet, Vertex}

object LinearExtrude {

  def apply(
    obj: Primitive[TwoDimensional],
    length: Double,
    rotation: Double = 0.0,
    slices: Int = 1
  ): Primitive3d = {
    val base = obj.rendered.facets
    Primitive3d(extrude(base, length, rotation, slices))
  }

  private def includeEdge(allEdges: Seq[(Vertex, Vertex)])(edge: (Vertex, Vertex)): Boolean = {
    val (a, b) = edge
    !allEdges.exists { case (othera, otherb) =>
      othera.approxEqual(b) && otherb.approxEqual(a)
    }
  }

  private def segments(base: Seq[Facet]): Seq[(Vertex, Vertex)] = {
    val allEdges = base.flatMap(_.edges)
    allEdges.filter(includeEdge(allEdges))
  }

  def extrude(base: IndexedSeq[Facet], length: Double, rotation: Double, slices: Int): IndexedSeq[Facet] = {
    def positionVertex(i: Int, v: Vertex): Vertex = {
      val angle = i * rotation
      Vertex(
        v.x * math.cos(angle) - v.y * math.sin(angle),
        v.x * math.sin(angle) + v.y * math.cos(angle),
        i * length / slices
      )
    }

    val perimeter = segments(base)
    val sides = Vector.range(0, slices).foldLeft(Vector.empty[Facet]) { (prev, i) =>
      prev ++ perimeter.flatMap { case (base1, base2) =>
        val b1 = positionVertex(i, base1)
        val b2 = positionVertex(i, base2)
        val t1 = positionVertex(i + 1, base1)
        val t2 = positionVertex(i + 1, base2)
        Vector(
          Facet(b1, t1, b2),
          Facet(b2, t1, t2)
        )
      }
    }
    val top = base.map { facet =>
      Facet(positionVertex(slices, facet.v3), positionVertex(slices, facet.v2), positionVertex(slices, facet.v1))
    }
    base ++ sides ++ top
  }
}