package net.joewing.scalacad.primitives

import net.joewing.scalacad.{Facet, Polygon, Vertex}

sealed trait Dim

class TwoDimensional extends Dim

class ThreeDimensional extends Dim

trait Primitive[T <: Dim] {

  private def reduceVertices(op: (Double, Double) => Double): Vertex = {
    val vs = render.flatMap(_.vertices)
    vs.tail.foldLeft(vs.head) { (b, v) =>
      Vertex(op(b.x1, v.x1), op(b.x2, v.x2), op(b.x3, v.x3))
    }
  }

  lazy val minBound: Vertex = reduceVertices(math.min)
  lazy val maxBound: Vertex = reduceVertices(math.max)
  lazy val extent: Vertex = maxBound - minBound

  def render: Seq[Facet]
}

