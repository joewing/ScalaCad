package net.joewing.scalacad.primitives

import net.joewing.scalacad.{RenderedObject, Vertex}

case class Polygon(points: Seq[(Double, Double)]) extends Primitive2d {
  lazy val render: RenderedObject = RenderedObject.fromVertices(points.map(p => Vertex(p._1, p._2, 0)))
}
