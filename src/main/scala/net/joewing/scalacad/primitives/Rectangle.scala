package net.joewing.scalacad.primitives

import net.joewing.scalacad.{RenderedObject, Vertex}

case class Rectangle(width: Double, height: Double) extends Primitive2d {
  lazy val render: RenderedObject = RenderedObject.fromVertices(
    Seq(Vertex(0, 0, 0), Vertex(0, height, 0), Vertex(width, height, 0), Vertex(width, 0, 0))
  )
}
