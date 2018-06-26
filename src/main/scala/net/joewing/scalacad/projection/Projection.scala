package net.joewing.scalacad.projection

import net.joewing.scalacad.Vertex

trait Projection {
  def project(v: Vertex): (Double, Double)
}
