package net.joewing.scalacad.primitives

import net.joewing.scalacad.{RenderedObject, Vertex}

final case class Rotate[D <: Dim](obj: Primitive[D], x: Double = 0, y: Double = 0, z: Double = 0) extends Primitive[D] {
  val dim: D = obj.dim

  protected def render: RenderedObject = obj.rendered.map(_.rotated(x, y, z))

  override def transformed(f: Primitive[D] => Primitive[D]): Primitive[D] =
    obj.transformed(o => f(Rotate(o, x, y, z)))

  lazy val minBound: Vertex = reduceVertices(rendered.vertices, _ min _)
  lazy val maxBound: Vertex = reduceVertices(rendered.vertices, _ max _)
}

