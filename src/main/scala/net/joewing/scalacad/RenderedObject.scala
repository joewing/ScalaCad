package net.joewing.scalacad

import net.joewing.scalacad.primitives.{Dim, LinearExtrude, ThreeDimensional, TwoDimensional}

import scala.concurrent.{ExecutionContext, Future}

sealed trait RenderedObject {
  implicit val dim: Dim

  def facets: Seq[Facet]
  def treeFuture(implicit ec: ExecutionContext): Future[BSPTree]

  def invert: RenderedObject

  def merge(other: RenderedObject)(implicit ec: ExecutionContext): Future[RenderedObject]

  final def union(other: RenderedObject)(implicit ec: ExecutionContext): Future[BSPTreeRenderedObject] = {
    val leftFuture = treeFuture
    val rightFuture = other.treeFuture
    for {
      left <- leftFuture
      right <- rightFuture
      leftClipped <- left.clip(right)
      rightClipped <- right.clip(leftClipped)
      invertClipped <- rightClipped.inverted.clip(leftClipped)
      merged <- leftClipped.merge(invertClipped.inverted)
    } yield BSPTreeRenderedObject(dim, merged)
  }

  final def intersect(other: RenderedObject)(implicit ec: ExecutionContext): Future[RenderedObject] = {
    invert.union(other.invert).map(_.invert)
  }

  final def minus(other: RenderedObject)(implicit ec: ExecutionContext): Future[RenderedObject] = {
    invert.union(other).map(_.invert)
  }

  final def map(f: Facet => Facet): FacetRenderedObject = RenderedObject.fromFacets(facets.map(f))

  final def filter(f: Facet => Boolean): FacetRenderedObject = RenderedObject.fromFacets(facets.filter(f))

  final def filterNot(f: Facet => Boolean): FacetRenderedObject = RenderedObject.fromFacets(facets.filterNot(f))
}

final case class FacetRenderedObject(dim: Dim, facets: Seq[Facet]) extends RenderedObject {
  def treeFuture(implicit ec: ExecutionContext): Future[BSPTree] = {
    dim match {
      case _: TwoDimensional => BSPTree(
        LinearExtrude.extrude(facets, 1, 0, 1).map(f => Polygon3d(f.vertices))
      )
      case _: ThreeDimensional => BSPTree(facets.map(f => Polygon3d(f.vertices)))
    }
  }

  def invert: RenderedObject = FacetRenderedObject(dim, facets.map(_.flip))

  def merge(other: RenderedObject)(implicit ec: ExecutionContext): Future[RenderedObject] = {
    other match {
      case rightTree: BSPTreeRenderedObject =>
        for {
          leftTree <- treeFuture
          merged <- leftTree.merge(rightTree.tree)
        } yield BSPTreeRenderedObject(dim, merged)
      case f: FacetRenderedObject   => Future(FacetRenderedObject(dim, facets ++ f.facets))
    }
  }
}

final case class BSPTreeRenderedObject(dim: Dim, tree: BSPTree) extends RenderedObject {
  def facets: Seq[Facet] = {
    val polygons = dim match {
      case _: TwoDimensional => tree.allPolygons.filter(_.vertices.forall(v => math.abs(v.z) < Vertex.epsilon))
      case _: ThreeDimensional => tree.allPolygons
    }
    val vertices = polygons.par.flatMap(_.vertices).seq.distinct
    val octree = Octree(vertices)
    polygons.par.flatMap { p =>
      Facet.fromVertices(p.vertices).flatMap(f => RenderedObject.insertPoints(f, octree).filter(RenderedObject.validFacet))
    }.seq
  }

  def treeFuture(implicit ec: ExecutionContext): Future[BSPTree] = Future.successful(tree)

  def invert: RenderedObject = BSPTreeRenderedObject(dim, tree.inverted)

  def merge(other: RenderedObject)(implicit ec: ExecutionContext): Future[RenderedObject] = for {
    otherTree <- other.treeFuture
    merged <- tree.merge(otherTree)
  }  yield BSPTreeRenderedObject(dim, merged)
}

object RenderedObject {

  def fromFacets(facets: Seq[Facet])(implicit dim: Dim): FacetRenderedObject = {
    FacetRenderedObject(dim, facets)
  }

  def fromVertices(vertices: Seq[Vertex])(implicit dim: Dim): FacetRenderedObject = {
    fromFacets(Facet.fromVertices(vertices))
  }

  // Insert a point to the facet by splitting it.
  // Note that this assumes all inserted points are on an edge of the facet.
  def insertPoint(facet: Facet, p: Vertex): Seq[Facet] = {
    if (p.between(facet.v1, facet.v2) && p.collinear(facet.v1, facet.v2)) {
      Vector(Facet(facet.v1, p, facet.v3), Facet(p, facet.v2, facet.v3))
    } else if (p.between(facet.v2, facet.v3) && p.collinear(facet.v2, facet.v3)) {
      Vector(Facet(facet.v1, facet.v2, p), Facet(p, facet.v3, facet.v1))
    } else if (p.between(facet.v3, facet.v1) && p.collinear(facet.v3, facet.v1)) {
      Vector(Facet(facet.v1, facet.v2, p), Facet(facet.v2, facet.v3, p))
    } else {
      Vector(facet)
    }
  }

  // Insert points from the Octree into the facet by splitting it.
  def insertPoints(facet: Facet, octree: Octree): Seq[Facet] = {
    val d = Vertex(Vertex.epsilon, Vertex.epsilon, Vertex.epsilon)
    octree.contained(facet.minBound - d, facet.maxBound + d).foldLeft(Vector(facet)) { (oldFacets, point) =>
      oldFacets.flatMap { f => insertPoint(f, point) }
    }
  }

  // Check if a facet is valid (has non-zero area).
  def validFacet(f: Facet): Boolean = {
    !f.v1.approxEqual(f.v2) && !f.v1.approxEqual(f.v3) && !f.v2.approxEqual(f.v3)
  }
}
