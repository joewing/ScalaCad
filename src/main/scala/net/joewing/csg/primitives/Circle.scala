package net.joewing.csg.primitives

import net.joewing.csg.{BSPTree, Polygon, Vertex}

case class Circle(r: Double, sides: Int) extends Primitive[TwoDimensional] {
  require(sides >= 3, s"sides must be at least 3 (got $sides)")
  def render: BSPTree = {
    val vs = Vector.tabulate(sides) { i =>
      val angle = -i * math.Pi * 2.0 / sides
      Vertex(r * math.cos(angle), r * math.sin(angle), 0)
    }
    BSPTree(Seq(Polygon(vs)))
  }
}
