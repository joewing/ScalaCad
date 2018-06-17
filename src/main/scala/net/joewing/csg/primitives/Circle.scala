package net.joewing.csg.primitives

import net.joewing.csg.{BSPTree, Facet, Vertex}

case class Circle(r: Double, sides: Int) extends Primitive[TwoDimensional] {
  require(sides >= 3, s"sides must be at least 3 (got $sides)")
  def render: BSPTree = {
    BSPTree(
      Vector.tabulate(sides) { i =>
        val angle1 = i * math.Pi * 2.0 / sides
        val angle2 = (i + 1) * math.Pi * 2.0 / sides
        Facet(
          Vertex(r * math.cos(angle2), r * math.sin(angle2), 0),
          Vertex(r * math.cos(angle1), r * math.sin(angle1), 0),
          Vertex(0, 0, 0)
        )
      }
    )
  }
}
