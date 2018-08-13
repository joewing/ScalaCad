package net.joewing.scalacad.primitives

import net.joewing.scalacad.Surface

case class Rotate[D <: Dim](obj: Primitive[D], x: Double = 0, y: Double = 0, z: Double = 0) extends Primitive[D] {
  lazy val render: Surface = obj.render.map(_.rotated(x, y, z))
}

