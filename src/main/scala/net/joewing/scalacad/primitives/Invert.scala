package net.joewing.scalacad.primitives

import net.joewing.scalacad.RenderedObject

case class Invert[D <: Dim](obj: Primitive[D]) extends Primitive[D] {
  lazy val render: RenderedObject = obj.render.invert
}
