package net.joewing.scalacad.primitives

import net.joewing.scalacad.{Polygon, Vertex}

case class Triangle(base: Double, height: Double) extends Primitive[TwoDimensional] {
  def render: Seq[Polygon] = Seq(Polygon(Seq(Vertex(0, 0, 0), Vertex(0, height, 0), Vertex(base, height, 0))))
}
