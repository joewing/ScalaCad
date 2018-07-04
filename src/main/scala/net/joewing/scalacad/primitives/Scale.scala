package net.joewing.scalacad.primitives

import net.joewing.scalacad.Polygon

case class Scale[D <: Dim](obj: Primitive[D], x: Double, y: Double, z: Double) extends Primitive[D] {
  def render: Seq[Polygon] = obj.render.map(_.scaled(x, y, z))
}

