package net.joewing.csg.primitives

import net.joewing.csg.{BSPTree, Facet, Vertex}

case class LinearExtrude(
  obj: Primitive[TwoDimensional],
  length: Double,
  rotation: Double = 0.0,
  slices: Int = 1
) extends Primitive[ThreeDimensional] {

  private def includeSide(base: Seq[Facet])(side: (Vertex, Vertex)): Boolean = {
    val (a, b) = side
    val count = base.foldLeft(0) { (c, f) =>
      val c1 = if (f.v1.approxEqual(a) && f.v2.approxEqual(b)) 1 else 0
      val c2 = if (f.v2.approxEqual(a) && f.v3.approxEqual(b)) 1 else 0
      val c3 = if (f.v3.approxEqual(a) && f.v1.approxEqual(b)) 1 else 0
      c + c1 + c2 + c3
    }
    count == 0
  }

  private def segments(base: Seq[Facet]): Seq[(Vertex, Vertex)] = {
    val vertices = base.flatMap(_.vertices)
    val pairs = vertices.zip(vertices.last +: vertices)
    pairs.filter(includeSide(base))
  }

  def render: BSPTree = {
    val base = obj.render.allFacets

    def positionVertex(i: Int, v: Vertex): Vertex = v.moved(z = i * length / slices).rotated(z = i * rotation)

    val perimeter = segments(base)
    val facets = Vector.range(0, slices).foldLeft(Seq.empty[Facet]) { (prevFacets, i) =>
      val sides = perimeter.flatMap { case (base1, base2) =>
        val b1 = positionVertex(i, base1)
        val b2 = positionVertex(i, base2)
        val t1 = positionVertex(i + 1, base1)
        val t2 = positionVertex(i + 1, base2)
        Seq(
          Facet(b1, b2, t1),
          Facet(b2, t2, t1)
        )
      }
      prevFacets ++ sides
    }
    val top = base.map(_.moved(z = length).rotated(z = rotation * slices).flip)
    BSPTree(base ++ facets ++ top)
  }
}
