package net.joewing.scalacad.primitives

import net.joewing.scalacad.{Polygon3d, RenderedObject, Vertex}

import scala.concurrent.{ExecutionContext, Future}

final case class Primitive2d(polygons: Seq[Polygon3d]) extends Primitive[TwoDimensional] {
  val dim: TwoDimensional = Dim.two

  protected def render(implicit ec: ExecutionContext): Future[RenderedObject] = {
    Future.successful(RenderedObject(dim, polygons))
  }

  override def extruded(
    extrude: Primitive[TwoDimensional] => Primitive[ThreeDimensional]
  ): Primitive[ThreeDimensional] = extrude(this)

  lazy val minBound: Vertex = reduceVertices(polygons.flatMap(_.vertices), _ min _)
  lazy val maxBound: Vertex = reduceVertices(polygons.flatMap(_.vertices), _ max _)
}
