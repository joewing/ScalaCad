package net.joewing.scalacad

object Utils {

  type Edge = (Vertex, Vertex)

  private def solve(values: (Double, Double, Double, Double, Double, Double, Double, Double)): Option[Double] = {
    val (ax, ay, bx, by, cx, cy, dx, dy) = values
    val top = bx * cy - bx * ay - ax * cy - by * cx + by * ax + ay * cx
    val bot = by * dx - by * cx - ay * dx + ay * cx - bx * dy + bx * cy + ax * dy - ax * cy
    if (math.abs(bot) > Vertex.epsilon) Some(top / bot) else None
  }

  def edgeIntersection(edge1: Edge, edge2: Edge): Option[Vertex] = {
    val (a, b) = edge1
    val (c, d) = edge2

    val values1 = Seq(
      (c.x1, c.x2, d.x1, d.x2, a.x1, a.x2, b.x1, b.x2),
      (c.x2, c.x3, d.x2, d.x3, a.x2, a.x3, b.x2, b.x3),
      (c.x3, c.x1, d.x3, d.x1, a.x3, a.x1, b.x3, b.x1)
    )
    val values2 = Seq(
      (a.x1, a.x2, b.x1, b.x2, c.x1, c.x2, d.x1, d.x2),
      (a.x2, a.x3, b.x2, b.x3, c.x2, c.x3, d.x2, d.x3),
      (a.x3, a.x1, b.x3, b.x1, c.x3, c.x1, d.x3, d.x1)
    )

    val sOpt = values1.flatMap(solve).headOption
    val tOpt = values2.flatMap(solve).headOption
    (sOpt, tOpt) match {
      case (Some(s), Some(t)) if s > -Vertex.epsilon && s < 1 + Vertex.epsilon &&
        t > -Vertex.epsilon && t < 1 + Vertex.epsilon =>
        Some(a + (b - a) * s)
      case _ => None
    }
  }

  // Get intersection point(s) for an edge and a facet if they are co-planar.
  def coplanarIntersectionEdges(edge: Edge, facet: Facet): Option[Edge] = {
    val contains1 = facet.contains(edge._1)
    val contains2 = facet.contains(edge._2)
    if (contains1 && contains2) {
      // If both end points are contained, we simply return them.
      Some(edge)
    } else if (contains1) {
      val other = facet.edges.flatMap(fe => edgeIntersection(fe, edge)).head
      Some(edge._1 -> other)
    } else if (contains2) {
      val other = facet.edges.flatMap(fe => edgeIntersection(fe, edge)).head
      Some(other -> edge._2)
    } else {
      val others = facet.edges.flatMap(fe => edgeIntersection(fe, edge))
      others match {
        case Seq(a, b) => Some(a -> b)
        case _ => None
      }
    }
  }

  def skewIntersection(edge: Edge, facet: Facet): Option[Vertex] = {
    val n = facet.normal
    val (a, b) = edge
    val bot = n.dot(b - a)
    if (math.abs(bot) > Vertex.epsilon) {
      val t = n.dot(facet.v1 - a) / bot
      val p = a + (b - a) * t
      if (t > -Vertex.epsilon && t < 1 + Vertex.epsilon && facet.contains(p)) Some(p) else None
    } else {
      // Parallel.
      None
    }
  }

  def distinctPoints(vs: Seq[Vertex]): Seq[Vertex] = {
    val result = scala.collection.mutable.ArrayBuffer[Vertex]()
    vs.foreach { v =>
      if (!result.exists(_.approxEqual(v))) result += v
    }
    result
  }

  // Get the intersecting segments formed between two facets (if any exist).
  def intersection(a: Facet, b: Facet): Seq[Edge] = {
    if (a.coplanar(b, Vertex.epsilon)) {
      // a and b are coplanar, so get the intersection points for each edge of a with b.
      val e1 = coplanarIntersectionEdges(a.v1 -> a.v2, b)
      val e2 = coplanarIntersectionEdges(a.v2 -> a.v3, b)
      val e3 = coplanarIntersectionEdges(a.v3 -> a.v1, b)
      e1.toSeq ++ e2.toSeq ++ e3.toSeq
    } else {
      // Not coplanar.  We can potentially get 1 point for each edge intersecting the other facet.
      // First check edges of a with b.  If we get two, we're done.
      // Otherwise, check b with a.
      val p1 = skewIntersection(a.v1 -> a.v2, b)
      val p2 = skewIntersection(a.v2 -> a.v3, b)
      val p3 = skewIntersection(a.v3 -> a.v1, b)
      val abPoints = distinctPoints(Seq(p1, p2, p3).flatten)
      if (abPoints.size == 2) {
        Seq(abPoints.head -> abPoints.last)
      } else {
        val p4 = skewIntersection(b.v1 -> b.v2, a)
        val p5 = skewIntersection(b.v2 -> b.v3, a)
        val p6 = skewIntersection(b.v3 -> b.v1, a)
        val baPoints = distinctPoints(Seq(p4, p5, p6).flatten)
        if (baPoints.size == 2) {
          Seq(baPoints.head -> baPoints.last)
        } else if (abPoints.nonEmpty && baPoints.nonEmpty) {
          Seq(abPoints.head -> baPoints.last)
        } else {
          Seq.empty
        }
      }
    }
  }

  def insertPoint(v1: Vertex, v2: Vertex, v3: Vertex, points: Seq[Vertex]): Seq[Facet] = {
    val p = points.head
    val remaining = points.tail
    val f = Facet(v1, v2, v3)
    insertPoints(f, remaining.filter(p => f.contains(p)))
  }

  def insertPoints(facet: Facet, points: Seq[Vertex]): Seq[Facet] = {
    if (points.nonEmpty) {
      val p = points.head
      if (p.approxEqual(facet.v1) || p.approxEqual(facet.v2) || p.approxEqual(facet.v3)) {
        insertPoints(facet, points.tail)
      } else if (p.collinear(facet.v1, facet.v2)) {
        val new2 = insertPoint(facet.v1, p, facet.v3, points)
        val new3 = insertPoint(p, facet.v2, facet.v3, points)
        new2 ++ new3
      } else if (p.collinear(facet.v2, facet.v3)) {
        val new1 = insertPoint(facet.v1, facet.v2, p, points)
        val new3 = insertPoint(p, facet.v3, facet.v1, points)
        new1 ++ new3
      } else if (p.collinear(facet.v3, facet.v1)) {
        val new1 = insertPoint(facet.v1, facet.v2, p, points)
        val new2 = insertPoint(facet.v2, facet.v3, p, points)
        new1 ++ new2
      } else {
        val new1 = insertPoint(facet.v1, facet.v2, p, points)
        val new2 = insertPoint(facet.v2, facet.v3, p, points)
        val new3 = insertPoint(p, facet.v3, facet.v1, points)
        new1 ++ new2 ++ new3
      }
    } else {
      Seq(facet)
    }
  }

  // Get edges that are missing, but required.
  def missingEdges(required: Seq[Edge], facets: Seq[Facet]): Seq[Edge] = {
    val allEdges = facets.flatMap(_.edges)
    required.filterNot { case (v1, v2) =>
      allEdges.exists { case (other1, other2) =>
        other1.approxEqual(v1) && other2.approxEqual(v2) || other1.approxEqual(v2) && other2.approxEqual(v1)
      }
    }
  }

  def inOrder(a: Vertex, b: Vertex, c: Vertex, d: Vertex): Boolean = b.between(a, c) && c.between(b, d)

  def divideCollinear(a: Vertex, x: Vertex, y: Vertex, b: Vertex, c: Vertex): Seq[Facet] = {
    if (inOrder(a, x, y, b)) {
      Seq(Facet(a, x, c), Facet(x, y, c), Facet(y, b, c))
    } else if (inOrder(a, y, x, b)) {
      Seq(Facet(a, y, c), Facet(y, x, c), Facet(x, b, c))
    } else if (inOrder(x, a, y, b) || y.between(a, b) && a.approxEqual(x)) {
      Seq(Facet(a, y, c), Facet(y, b, c))
    } else if (inOrder(y, a, x, b) || x.between(a, b) && a.approxEqual(y)) {
      Seq(Facet(a, x, c), Facet(x, b, c))
    } else if (inOrder(a, x, b, y) || x.between(a, b) && b.approxEqual(y)) {
      Seq(Facet(a, x, c), Facet(x, b, c))
    } else if (inOrder(a, y, b, x) || y.between(a, b) && b.approxEqual(x)) {
      Seq(Facet(a, y, c), Facet(y, b, c))
    } else {
      require(!x.between(a, b) && !y.between(a, b))
      Seq(Facet(a, b, c))
    }
  }

  def insertEdge(facet: Facet, edge: Edge): Seq[Facet] = {
    if (facet.v1.collinear(edge._1, edge._2) && facet.v2.collinear(edge._1, edge._2)) {
      divideCollinear(facet.v1, edge._1, edge._2, facet.v2, facet.v3)
    } else if (facet.v1.collinear(edge._1, edge._2) && facet.v3.collinear(edge._1, edge._2)) {
      divideCollinear(facet.v3, edge._1, edge._2, facet.v1, facet.v2)
    } else if (facet.v2.collinear(edge._1, edge._2) && facet.v3.collinear(edge._1, edge._2)) {
      divideCollinear(facet.v2, edge._1, edge._2, facet.v3, facet.v1)
    } else {
      facet.edges.map(fe => edgeIntersection(fe, edge)) match {
        case Seq(Some(a), Some(b), Some(c)) if a.approxEqual(b)  && a.approxEqual(c) =>
          Seq(facet)
        case Seq(Some(a), Some(b), Some(c)) if a.approxEqual(b) =>
          // Edge intersection through v3 -> v1 and vertex intersection through v2.
          Seq(Facet(facet.v1, facet.v2, c), Facet(c, facet.v2, facet.v3))
        case Seq(Some(a), Some(b), Some(c)) if a.approxEqual(c) =>
          // Edge intersection through v2 -> v3 and vertex intersection through v1.
          Seq(Facet(facet.v1, facet.v2, b), Facet(b, facet.v3, facet.v1))
        case Seq(Some(a), Some(b), Some(c)) if b.approxEqual(c) =>
          // Edge intersection through v1 -> v2 and vertex interesction through v3.
          Seq(Facet(facet.v1, a, facet.v3), Facet(a, facet.v2, facet.v3))
        case Seq(Some(a), Some(b), None) =>
          if (a.approxEqual(b)) {
            // Intersection at vertex v2.
            Seq(facet)
          } else {
            // Intersection between v1 -> v2 and v2 -> v3
            Seq(Facet(facet.v1, a, facet.v3), Facet(a, facet.v2, b), Facet(a, b, facet.v3))
          }
        case Seq(Some(a), None, Some(c)) =>
          if (a.approxEqual(c)) {
            Seq(facet)
          } else {
            // Intersection between v1 -> v2 and v3 -> v1
            Seq(Facet(facet.v1, a, c), Facet(a, facet.v2, c), Facet(facet.v2, facet.v3, c))
          }
        case Seq(None, Some(b), Some(c)) =>
          if (b.approxEqual(c)) {
            Seq(facet)
          } else {
            Seq(Facet(facet.v1, facet.v2, c), Facet(facet.v2, b, c), Facet(b, facet.v3, c))
          }
        case Seq(Some(a), None, None) =>
          // Single point of intersection through v1 -> v2
          Seq(Facet(facet.v1, a, facet.v3), Facet(a, facet.v2, facet.v3))
        case Seq(None, Some(b), None) =>
          // Single point of intersection through v2 -> v3
          Seq(Facet(facet.v1, facet.v2, b), Facet(b, facet.v3, facet.v1))
        case Seq(None, None, Some(c)) =>
          // Single point of intersection through v3 -> v1
          Seq(Facet(facet.v1, facet.v2, c), Facet(facet.v2, facet.v3, c))
        case _ =>
          // No intersection
          Seq(facet)
      }
    }
  }

  // Flip facets necessary to restore missing edges.
  def insertEdges(facets: Seq[Facet], required: Seq[Edge]): Seq[Facet] = {
    missingEdges(required, facets).foldLeft(facets) { (oldFacets, edge) =>
      oldFacets.flatMap { facet => insertEdge(facet, edge) }
    }
  }

  // Insert intersection points from others into facet.
  def insertIntersections(facet: Facet, others: Seq[Facet]): Seq[Facet] = {
    // Get other points that intersect this facet.
    val edges: Seq[Edge] = others.flatMap { other =>
      intersection(other, facet)
    }.map { case (v1, v2) =>
      // Arrange edges so the smallest comes first.
      if (Vertex.VertexOrdering.compare(v1, v2) < 0) (v1, v2) else (v2, v1)
    }.sortBy(_._2).sortBy(_._1)

    val uniqueEdges = edges.foldLeft(Vector.empty[Edge]) { case (lst, (v1, v2)) =>
      if (lst.nonEmpty && v1.approxEqual(lst.last._1) && v2.approxEqual(lst.last._2)) {
        lst
      } else {
        lst :+ (v1, v2)
      }
    }

    val points = distinctPoints(uniqueEdges.flatMap(e => Seq(e._1, e._2)))
    val initialTriangulation = insertPoints(facet, points)
    insertEdges(initialTriangulation, uniqueEdges.filter(e => !e._1.approxEqual(e._2)))
  }

  def insertIntersections(left: Seq[Facet], right: Seq[Facet]): (Seq[Facet], Seq[Facet]) = {
      val left1 = left.flatMap(f => insertIntersections(f, right))
      val right1 = right.flatMap(f => insertIntersections(f, left1))
      val left2 = left1.flatMap(f => insertIntersections(f, right1))
      left2 -> right1
  }

  // Find the facet containing p.
  def findFacet(facets: Seq[Facet], p: Vertex): Option[Facet] = facets.find(_.contains(p))

  // Determine if p is on the boundary of any facets.
  def onBoundary(facets: Seq[Facet], p: Vertex): Boolean = findFacet(facets, p).isDefined

  // Determine if p is contained in facets.
  def isContained(facets: Seq[Facet], p: Vertex): Boolean = {

    // Determine the max bound box for facets.
    val maxBound = facets.tail.foldLeft(facets.head.maxBound)(_ max _.maxBound) + Vertex(1, 2, 3)
    lazy val minBound = facets.tail.foldLeft(facets.head.minBound)(_ min _.minBound) - Vertex(1, 0, 1)

    // Create a line segment through p that extends past our bounding box.
    val edge1 = p -> maxBound
    lazy val edge2 = p -> minBound

    // Count the number of facets this edge intersects.
    val count1 = distinctPoints(facets.flatMap(facet => skewIntersection(edge1, facet))).size
    lazy val count2 = distinctPoints(facets.flatMap(facet => skewIntersection(edge2, facet))).size

    // The point is contained if it intersects the object an odd number of times.
    count1 % 2 == 1 || count2 % 2 == 1
  }

  // Get all unmatched edges.
  def unmatchedEdges(facets: Seq[Facet]): Seq[Edge] = {
    val unmatched = scala.collection.mutable.ArrayBuffer[Edge]()
    facets.foreach { facet =>
      facet.edges.foreach { edge =>
        val i = unmatched.indexWhere(other => other._1.approxEqual(edge._2) && other._2.approxEqual(edge._1))
        if (i >= 0) {
          unmatched.remove(i)
        } else {
          unmatched += edge
        }
      }
    }
    unmatched
  }

  // Make sure all edges are matched.
  def validateEdges(facets: Seq[Facet]): Seq[Facet] = {
    val unmatched = unmatchedEdges(facets)
    require(unmatched.isEmpty, s"unmatched edges: $unmatched")
    facets
  }

}
