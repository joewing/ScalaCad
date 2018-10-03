package net.joewing.scalacad.primitives

import net.joewing.scalacad.{RenderedObject, Vertex}

sealed trait Dim

class TwoDimensional extends Dim
class ThreeDimensional extends Dim

object Dim {
  val two: TwoDimensional = new TwoDimensional
  val three: ThreeDimensional = new ThreeDimensional
}

trait Primitive[D <: Dim] {

  final protected def reduceVertices(vertices: Seq[Vertex], op: (Double, Double) => Double): Vertex = {
    if (vertices.nonEmpty) {
      vertices.tail.foldLeft(vertices.head) { (b, v) =>
        Vertex(op(b.x, v.x), op(b.y, v.y), op(b.z, v.z))
      }
    } else {
      Vertex(0, 0, 0)
    }
  }

  val minBound: Vertex
  val maxBound: Vertex
  final lazy val extent: Vertex = maxBound - minBound

  val dim: D

  protected def render: RenderedObject
  def transformed(f: Primitive[D] => Primitive[D]): Primitive[D] = f(this)

  final lazy val rendered: RenderedObject = transformed(identity).render
}
