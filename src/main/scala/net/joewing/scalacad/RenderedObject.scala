package net.joewing.scalacad

import net.joewing.scalacad.primitives.{Dim, LinearExtrude, ThreeDimensional, TwoDimensional}

sealed trait RenderedObject {
  implicit val dim: Dim
  def vertices: Seq[Vertex]

  def facets: Seq[Facet]
  def tree: BSPTree

  def invert: RenderedObject

  def map(f: Facet => Facet): FacetRenderedObject = RenderedObject.fromFacets(facets.map(f))

  def filter(f: Facet => Boolean): FacetRenderedObject = RenderedObject.fromFacets(facets.filter(f))

  def filterNot(f: Facet => Boolean): FacetRenderedObject = RenderedObject.fromFacets(facets.filterNot(f))
}

final case class FacetRenderedObject(dim: Dim, facets: Seq[Facet]) extends RenderedObject {
  lazy val vertices: Seq[Vertex] = facets.flatMap(_.vertices).distinct

  def tree: BSPTree = dim match {
    case _: TwoDimensional => BSPTree(
      LinearExtrude.extrude(facets, 1, 0, 1).map(PlanePolygon.fromFacet)
    )
    case _: ThreeDimensional => BSPTree(facets.map(PlanePolygon.fromFacet))
  }

  def invert: RenderedObject = FacetRenderedObject(dim, facets.map(_.flip))
}

final case class BSPTreeRenderedObject(dim: Dim, tree: BSPTree) extends RenderedObject {
  lazy val vertices: Seq[Vertex] = tree.allPolygons.flatMap(_.vertices)

  def facets: Seq[Facet] = RenderedObject.treeToFacets(dim, tree)

  def invert: RenderedObject = BSPTreeRenderedObject(dim, tree.inverted)
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

  def treeToFacets(dim: Dim, root: BSPTree): Seq[Facet] = {
    val polygons = dim match {
      case _: TwoDimensional =>
        root.allPolygons.filter(_.vertices.forall(v => math.abs(v.z) < Vertex.epsilon))
      case _: ThreeDimensional => root.allPolygons
    }
    val vertices = polygons.flatMap(_.vertices).distinct
    val octree = Octree(vertices)
    polygons.par.flatMap { p =>
      Facet.fromVertices(p.vertices).flatMap(f => insertPoints(f, octree).filter(validFacet))
    }.seq
  }
}
