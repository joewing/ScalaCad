package net.joewing.csg.primitives

import net.joewing.csg.BSPTree

trait Renderable {
  def render: BSPTree
}

trait Solid extends Renderable

trait Surface extends Renderable

trait Operation extends Renderable
