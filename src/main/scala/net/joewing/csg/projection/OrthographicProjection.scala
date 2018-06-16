package net.joewing.csg.projection

import net.joewing.csg.Vertex

object OrthographicProjection extends Projection {
  def project(v: Vertex): (Double, Double) = {
    v.x1 -> v.x2
  }
}
