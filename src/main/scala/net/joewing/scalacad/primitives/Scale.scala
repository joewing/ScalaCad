package net.joewing.scalacad.primitives

import net.joewing.scalacad.Surface

case class Scale[D <: Dim](obj: Primitive[D], x: Double, y: Double, z: Double) extends Primitive[D] {
  lazy val render: Surface = obj.render.map(_.scaled(x, y, z))
}

