package net.joewing.csg.primitives

import net.joewing.csg.{BSPTree, Facet}

case class LinearExtrude(
  obj: Primitive[TwoDimensional],
  length: Double,
  rotation: Double = 0.0,
  slices: Int = 1
) extends Primitive[ThreeDimensional] {
  def render: BSPTree = {
    val base = obj.render.allFacets
    val init: (Seq[Facet], Seq[Facet]) = (base, Seq.empty)
    val facets = Vector.range(1, slices + 1).foldLeft(init) { case ((bottom, prevFacets), i) =>
      val top = bottom.map(_.moved(z = length / slices).rotated(z = rotation))
      val sides = bottom.zip(top).flatMap { case (b, t) =>
        Vector(
          Facet(t.v1, b.v2, b.v1),
          Facet(t.v1, t.v2, b.v2),
          Facet(t.v2, b.v3, b.v2),
          Facet(t.v2, t.v3, b.v3),
          Facet(t.v3, b.v1, b.v3),
          Facet(t.v3, t.v1, b.v1)
        )
      }
      if (slices == 1) {
        // Top and bottom
        (top, prevFacets ++ bottom ++ top.map(_.flip) ++ sides)
      } else if (i == 1) {
        // Bottom
        (top, prevFacets ++ bottom ++ sides)
      } else if (i == slices) {
        // Top
        (top, prevFacets ++ top.map(_.flip) ++ sides)
      } else {
        // Middle
        (top, prevFacets ++ sides)
      }
    }._2
    BSPTree(facets)
  }
}
