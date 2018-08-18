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

  private def reduceVertices(op: (Double, Double) => Double): Vertex = {
    val vertices = render.vertices
    if (vertices.nonEmpty) {
      vertices.tail.foldLeft(vertices.head) { (b, v) =>
        Vertex(op(b.x, v.x), op(b.y, v.y), op(b.z, v.z))
      }
    } else {
      Vertex(0, 0, 0)
    }
  }

  lazy val minBound: Vertex = reduceVertices(math.min)
  lazy val maxBound: Vertex = reduceVertices(math.max)
  lazy val extent: Vertex = maxBound - minBound

  implicit val dim: D

  def render: RenderedObject
}

trait Primitive2d extends Primitive[TwoDimensional] {
  implicit val dim: TwoDimensional = Dim.two
}

trait Primitive3d extends Primitive[ThreeDimensional] {
  implicit val dim: ThreeDimensional = Dim.three
}

