package net.joewing.scalacad.primitives

import net.joewing.scalacad.{Facet, FacetRenderedObject, RenderedObject, Vertex}

import scala.concurrent.{ExecutionContext, Future}

final case class Primitive3d(facets: Seq[Facet]) extends Primitive[ThreeDimensional] {
  val dim: ThreeDimensional = Dim.three

  protected def render(implicit ec: ExecutionContext): Future[RenderedObject] = {
    Future.successful(FacetRenderedObject(dim, facets))
  }

  lazy val minBound: Vertex = reduceVertices(facets.flatMap(_.vertices), _ min _)
  lazy val maxBound: Vertex = reduceVertices(facets.flatMap(_.vertices), _ max _)
}
