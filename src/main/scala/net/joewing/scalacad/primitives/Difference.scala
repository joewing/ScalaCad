package net.joewing.scalacad.primitives

import net.joewing.scalacad._

case class Difference[D <: Dim](base: Primitive[D], minus: Primitive[D]) extends Primitive[D] {
  lazy val render: RenderedObject = Invert(Union(Invert(base), minus)).render
}

