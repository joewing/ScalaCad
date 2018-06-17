package net.joewing.csg.primitives

import net.joewing.csg.BSPTree

sealed trait Dim

class TwoDimensional extends Dim

class ThreeDimensional extends Dim

trait Primitive[T <: Dim] {
  def render: BSPTree
}

