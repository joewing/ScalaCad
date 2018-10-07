package net.joewing.scalacad.primitives

import net.joewing.scalacad.{RenderedObject, Vertex}

final case class Rotate[D <: Dim](obj: Primitive[D], x: Double = 0, y: Double = 0, z: Double = 0) extends Primitive[D] {
  val dim: D = obj.dim

  protected def render: RenderedObject = obj.rendered.map(_.rotated(x, y, z))

  override def transformed(f: Primitive[D] => Primitive[D]): Primitive[D] =
    obj.transformed(o => f(Rotate(o, x, y, z)))

  private lazy val boundingVertices: Seq[Vertex] = Seq(
    Vertex(obj.minBound.x, obj.minBound.y, obj.minBound.z),
    Vertex(obj.maxBound.x, obj.minBound.y, obj.minBound.z),
    Vertex(obj.minBound.x, obj.maxBound.y, obj.minBound.z),
    Vertex(obj.maxBound.x, obj.maxBound.y, obj.minBound.z),
    Vertex(obj.minBound.x, obj.minBound.y, obj.maxBound.z),
    Vertex(obj.maxBound.x, obj.minBound.y, obj.maxBound.z),
    Vertex(obj.minBound.x, obj.maxBound.y, obj.maxBound.z),
    Vertex(obj.maxBound.x, obj.maxBound.y, obj.maxBound.z)
  ).map(_.rotated(x, y, z))

  lazy val minBound: Vertex = reduceVertices(boundingVertices, _ min _)
  lazy val maxBound: Vertex = reduceVertices(boundingVertices, _ max _)
}

