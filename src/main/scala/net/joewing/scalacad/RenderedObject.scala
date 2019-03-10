package net.joewing.scalacad

import net.joewing.scalacad.primitives.Dim

import scala.concurrent.{ExecutionContext, Future}

final case class RenderedObject(dim: Dim, polygons: Seq[Polygon3d]) {

  lazy val minBound: Vertex = polygons.foldLeft(Vertex.max) { (v, p) => p.minBound.min(v) }
  lazy val maxBound: Vertex = polygons.foldLeft(Vertex.min) { (v, p) => p.maxBound.max(v) }

  def map(f: Polygon3d => Polygon3d): RenderedObject = RenderedObject(dim, polygons.map(f))

  def overlaps(right: RenderedObject): Boolean = {
    val (mina, maxa) = (minBound, maxBound)
    val (minb, maxb) = (right.minBound, right.maxBound)
    !(maxa.x < minb.x || maxa.y < minb.y || maxa.z < minb.z || mina.x > maxb.x || mina.y > maxb.y || mina.z > maxb.z)
  }

  def union(other: RenderedObject)(implicit ec: ExecutionContext): Future[RenderedObject] = {
    val leftFuture = treeFuture
    val rightFuture = other.treeFuture
    for {
      left <- leftFuture
      right <- rightFuture
      leftClipped <- left.clip(right)
      rightClipped <- right.clip(leftClipped)
      invertClipped <- rightClipped.inverted.clip(leftClipped)
      merged = leftClipped.allPolygons ++ invertClipped.inverted.allPolygons
    } yield RenderedObject(dim, merged)
  }

  def intersect(other: RenderedObject)(implicit ec: ExecutionContext): Future[RenderedObject] = {
    invert.union(other.invert).map(_.invert)
  }

  def minus(other: RenderedObject)(implicit ec: ExecutionContext): Future[RenderedObject] = {
    invert.union(other).map(_.invert)
  }

  def treeFuture(implicit ec: ExecutionContext): Future[BSPTree] = BSPTree(polygons.toIndexedSeq)

  def facets: IndexedSeq[Facet] = {
    val vertices = polygons.flatMap(_.vertices).distinct
    val octree = Octree(vertices)
    polygons.par.flatMap { p =>
      Facet.fromVertices(p.vertices).flatMap(f => RenderedObject.insertPoints(f, octree))
    }.seq.toIndexedSeq
  }

  def invert: RenderedObject = RenderedObject(dim, polygons.map(_.flip))

  def merge(other: RenderedObject): RenderedObject = RenderedObject(dim, polygons ++ other.polygons)
}

object RenderedObject {

  def fromVertices(vertices: Seq[Vertex])(implicit dim: Dim): RenderedObject = {
    RenderedObject(dim, Array(Polygon3d(vertices)))
  }

  // Insert a point to the facet by splitting it.
  // Note that this assumes all inserted points are on an edge of the facet.
  def insertPoint(facet: Facet, p: Vertex): Seq[Facet] = {
    if (p.between(facet.v1, facet.v2)) {
      Vector(Facet(facet.v1, p, facet.v3), Facet(p, facet.v2, facet.v3))
    } else if (p.between(facet.v2, facet.v3)) {
      Vector(Facet(facet.v1, facet.v2, p), Facet(p, facet.v3, facet.v1))
    } else if (p.between(facet.v3, facet.v1)) {
      Vector(Facet(facet.v1, facet.v2, p), Facet(facet.v2, facet.v3, p))
    } else {
      Vector(facet)
    }
  }

  // Insert points from the Octree into the facet by splitting it.
  def insertPoints(facet: Facet, octree: Octree): Seq[Facet] = {
    val d = Vertex(Vertex.epsilon, Vertex.epsilon, Vertex.epsilon)
    octree.contained(facet.minBound - d, facet.maxBound + d).foldLeft(Vector(facet)) { (oldFacets, point) =>
      oldFacets.flatMap { f => insertPoint(f, point).filter(validFacet) }
    }
  }

  // Check if a facet is valid (has non-zero area).
  def validFacet(f: Facet): Boolean = {
    !f.v1.approxEqual(f.v2) && !f.v1.approxEqual(f.v3) && !f.v2.approxEqual(f.v3)
  }
}
