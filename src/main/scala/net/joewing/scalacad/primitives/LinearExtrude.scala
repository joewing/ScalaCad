package net.joewing.scalacad.primitives

import net.joewing.scalacad.{Facet, RenderedObject, Vertex}

import scala.concurrent.{ExecutionContext, Future}

final case class LinearExtrude(
  obj: Primitive[TwoDimensional],
  length: Double,
  slices: Int,
  func: (Int, Vertex) => Vertex,
) extends Primitive[ThreeDimensional] {
  implicit val dim: ThreeDimensional = Dim.three

  protected def render(implicit ec: ExecutionContext): Future[RenderedObject] = {
    obj.renderedFuture.map { base =>
      RenderedObject.fromFacets(LinearExtrude.extrude(base.facets, length, slices, func))
    }
  }

  override def transformed(
    f: Primitive[ThreeDimensional] => Primitive[ThreeDimensional]
  ): Primitive[ThreeDimensional] =  {
    obj.extruded(o => f(LinearExtrude(o, length, slices, func)))
  }

  override def extruded(
    f: Primitive[TwoDimensional] => Primitive[ThreeDimensional]
  ): Primitive[ThreeDimensional] = throw new IllegalStateException(s"cannot extrude 3d primitive")

  lazy val minBound: Vertex = obj.minBound.copy(z = 0)
  lazy val maxBound: Vertex = obj.maxBound.copy(z = length)
}

object LinearExtrude {

  def moveFunc(slices: Int, length: Double)(i: Int, v: Vertex): Vertex = {
    Vertex(v.x, v.y, v.z + i * length / slices)
  }

  def rotateFunc(angle: Double)(i: Int, v: Vertex): Vertex = {
    val theta = i * angle
    Vertex(
      v.x * math.cos(theta) - v.y * math.sin(theta),
      v.x * math.sin(theta) + v.y * math.cos(theta),
      v.z
    )
  }

  def zoomFunc(scalex: Double, scaley: Double)(i: Int, v: Vertex): Vertex = {
    Vertex(v.x * math.pow(scalex, i), v.y * math.pow(scaley, i), v.z)
  }

  def composeFunc(fs: (Int, Vertex) => Vertex*)(i: Int, v: Vertex): Vertex = {
    fs.foldLeft(v) { (c, f) => f(i, c) }
  }

  def apply(
    obj: Primitive[TwoDimensional],
    length: Double,
    slices: Int = 1,
    rotation: Double = 0.0,
    scalex: Double = 1.0,
    scaley: Double = 1.0
  ): Primitive[ThreeDimensional] = {
    LinearExtrude(obj, length, slices,
      composeFunc(moveFunc(slices, length), rotateFunc(rotation), zoomFunc(scalex, scaley))(_, _)
    )
  }

  private def includeEdge(allEdges: Seq[(Vertex, Vertex)])(edge: (Vertex, Vertex)): Boolean = {
    val (a, b) = edge
    !allEdges.exists { case (othera, otherb) =>
      othera.approxEqual(b) && otherb.approxEqual(a)
    }
  }

  private def segments(base: Seq[Facet]): Seq[(Vertex, Vertex)] = {
    val allEdges = base.flatMap(_.edges)
    allEdges.filter(includeEdge(allEdges))
  }

  def extrude(
    base: IndexedSeq[Facet],
    length: Double,
    slices: Int,
    func: (Int, Vertex) => Vertex
  ): IndexedSeq[Facet] = {
    val perimeter = segments(base)
    val sides = Vector.range(0, slices).foldLeft(Vector.empty[Facet]) { (prev, i) =>
      prev ++ perimeter.flatMap { case (base1, base2) =>
        val b1 = func(i, base1)
        val b2 = func(i, base2)
        val t1 = func(i + 1, base1)
        val t2 = func(i + 1, base2)
        Vector(
          Facet(b1, t1, b2),
          Facet(b2, t1, t2)
        )
      }
    }
    val top = base.map { facet =>
      Facet(func(slices, facet.v3), func(slices, facet.v2), func(slices, facet.v1))
    }
    base ++ sides ++ top
  }
}
