package net.joewing.scalacad.primitives

import net.joewing.scalacad.Surface

case class Invert[D <: Dim](obj: Primitive[D]) extends Primitive[D] {
  lazy val render: Surface = obj.render.invert
}
