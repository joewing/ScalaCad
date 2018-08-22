package net.joewing.scalacad.primitives

import net.joewing.scalacad.RenderedObject

case class Translate[D <: Dim](obj: Primitive[D], x: Double = 0, y: Double = 0, z: Double = 0) extends Primitive[D] {
  implicit val dim: D = obj.dim
  protected def render: RenderedObject = obj.rendered.map(_.moved(x, y, z))

  override def transformed(f: Primitive[D] => Primitive[D]): Primitive[D] =
    obj.transformed(o => f(Translate(o, x, y, z)))
}

