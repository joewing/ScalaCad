package net.joewing.scalacad.primitives

import net.joewing.scalacad.{Surface, Vertex}

sealed trait Dim

class TwoDimensional extends Dim

class ThreeDimensional extends Dim

trait Primitive[T <: Dim] {

  private def reduceVertices(op: (Double, Double) => Double): Vertex = {
    val vertices = render.vertices
    vertices.tail.foldLeft(vertices.head) { (b, v) =>
      Vertex(op(b.x, v.x), op(b.y, v.y), op(b.z, v.z))
    }
  }

  lazy val minBound: Vertex = reduceVertices(math.min)
  lazy val maxBound: Vertex = reduceVertices(math.max)
  lazy val extent: Vertex = maxBound - minBound

  def render: Surface
}

