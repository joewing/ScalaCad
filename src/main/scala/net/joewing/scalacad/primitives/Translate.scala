package net.joewing.scalacad.primitives

import net.joewing.scalacad.RenderedObject

case class Translate[D <: Dim](obj: Primitive[D], x: Double = 0, y: Double = 0, z: Double = 0) extends Primitive[D] {
  lazy val render: RenderedObject = obj.render.map(_.moved(x, y, z))
}

