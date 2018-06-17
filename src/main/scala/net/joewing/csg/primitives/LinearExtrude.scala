package net.joewing.csg.primitives

import net.joewing.csg.{BSPTree, Facet, Vertex}

case class LinearExtrude(obj: Primitive[TwoDimensional], length: Double) extends Primitive[ThreeDimensional] {
  def render: BSPTree = {
    val base = obj.render.allFacets
    val top = base.map(_.moved(z = length))
    val sides = base.zip(top).flatMap { case (b, t) =>
      Vector(
        Facet(t.v1, b.v2, b.v1),
        Facet(t.v1, t.v2, b.v2),
        Facet(t.v2, b.v3, b.v2),
        Facet(t.v2, t.v3, b.v3),
        Facet(t.v3, b.v1, b.v3),
        Facet(t.v3, t.v1, b.v1)
      )
    }
    BSPTree(base ++ top.map(_.flip) ++ sides)
  }
}
