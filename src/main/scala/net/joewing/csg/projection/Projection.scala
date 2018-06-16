package net.joewing.csg.projection

import net.joewing.csg.Vertex

trait Projection {
  def project(v: Vertex): (Double, Double)
}
