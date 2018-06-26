package net.joewing.scalacad.projection

import net.joewing.scalacad.Vertex

object OrthographicProjection extends Projection {
  def project(v: Vertex): (Double, Double) = {
    v.x1 -> v.x2
  }
}
