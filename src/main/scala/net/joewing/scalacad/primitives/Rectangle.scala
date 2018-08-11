package net.joewing.scalacad.primitives

import net.joewing.scalacad.{Surface, Vertex}

case class Rectangle(width: Double, height: Double) extends Primitive[TwoDimensional] {
  def render: Surface = Surface.fromVertices(
    Seq(Vertex(0, 0, 0), Vertex(0, height, 0), Vertex(width, height, 0), Vertex(width, 0, 0))
  )
}
