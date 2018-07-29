package net.joewing.scalacad

object Delaunay {

  def angle(a: Vertex, b: Vertex, x: Vertex): Double = {
    val ax = a - x
    val bx = b - x
    math.acos(ax.dot(bx) / (ax.length * bx.length))
  }

  def smallestAngle(facet: Facet): Double = {
    val theta1 = angle(facet.v1, facet.v2, facet.v3)
    val theta2 = angle(facet.v2, facet.v3, facet.v1)
    val theta3 = angle(facet.v3, facet.v1, facet.v2)
    math.min(theta1, math.min(theta2, theta3))
  }

  def smallestAngle(a: Facet, b: Facet): Double = math.min(smallestAngle(a), smallestAngle(b))

  /** Flip the edge shared between two facets.
   * Note that this will not preserve the normal.
   */
  def flip(a: Facet, b: Facet, v1: Vertex, v2: Vertex): (Facet, Facet) = {

    // v1 -> v2 is the edge shared by a and b.  So first find the unshared edges.
    val unshared1 = a.vertices.find(v => !v.approxEqual(v1) && !v.approxEqual(v2)).get
    val unshared2 = b.vertices.find(v => !v.approxEqual(v1) && !v.approxEqual(v2)).get

    if (unshared1.collinear(unshared2, v2) || unshared2.collinear(unshared1, v1)) {
      // Flipping would create an invalid facet.
      a -> b
    } else {
      // Create the new facets.
      // unshared1 -> unshared2 will become the new shared edge.
      val newa = Facet(unshared1, unshared2, v2)
      val newb = Facet(v1, unshared2, unshared1)
      newa -> newb
    }
  }

  def isLegal(a: Vertex, b: Vertex, facets: Seq[Facet]): Boolean = {

    // Find the two facets sharing this edge.
    // TODO: this is likely way too slow.
    val leftOpt = facets.find { f =>
      f.edges.exists(e => e._1.approxEqual(a) && e._2.approxEqual(b))
    }
    val rightOpt = facets.find { f =>
      f.edges.exists(e => e._1.approxEqual(b) && e._2.approxEqual(a))
    }

    (leftOpt, rightOpt) match {
      case (Some(left), Some(right)) =>

        // Get the smallest angle of the existing edge.
        val angleExisting = smallestAngle(left, right)

        // Get the smallest angle if we were to flip the edge.
        val (flippedLeft, flippedRight) = flip(left, right, a, b)
        val angleFlipped = smallestAngle(flippedLeft, flippedRight)

        // The edge is legal if the smallest angle of the flipped set <= the smallest angle of the existing set
        angleFlipped <= angleExisting

      case _ => true
    }
  }

  def insertFacet(pr: Vertex, pi: Vertex, pj: Vertex, base: Facet, facets: scala.collection.mutable.ArrayBuffer[Facet]): Unit = {
    val f = Facet(pr, pi, pj)
    if (base.normal.dot(f.normal) < 0) {
      facets += f.flip
    } else {
      facets += f
    }
  }

  def legalizeEdge(pr: Vertex, pi: Vertex, pj: Vertex, base: Facet, facets: scala.collection.mutable.ArrayBuffer[Facet]): Unit = {
    if (!isLegal(pi, pj, facets)) {
      // Illegal; flip the edge and legalize the new edges.
      val index1 = facets.indexWhere { f =>
        f.vertices.exists(_.approxEqual(pr)) &&
        f.vertices.exists(_.approxEqual(pi)) &&
        f.vertices.exists(_.approxEqual(pj))
      }
      val olda = facets(index1)
      facets.remove(index1)

      val index2 = facets.indexWhere { f =>
        f.vertices.exists(_.approxEqual(pi)) && f.vertices.exists(_.approxEqual(pj))
      }
      val oldb = facets(index2)
      facets.remove(index2)

      val pk = oldb.vertices.find(v => !v.approxEqual(pi) && !v.approxEqual(pj)).get

      // Replace pi -> pj with pr -> pk.
      legalizeEdge(pr, pi, pk, base, facets)
      legalizeEdge(pr, pk, pj, base, facets)
      insertFacet(pr, pi, pk, base, facets)
      insertFacet(pr, pk, pj, base, facets)
    }
  }

  def removeFacet(v1: Vertex, v2: Vertex, facets: scala.collection.mutable.ArrayBuffer[Facet]): Option[Facet] = {
    val i = facets.indexWhere { f =>
      f.edges.exists { e =>
        e._1.approxEqual(v1) && e._2.approxEqual(v2) || e._1.approxEqual(v2) && e._2.approxEqual(v1)
      }
    }
    if (i >= 0) {
      val facet = facets(i)
      facets.remove(i)
      Some(facet)
    } else None
  }

  def addCollinear(
    pr: Vertex,
    pi: Vertex,
    pj: Vertex,
    pk: Vertex,
    base: Facet,
    facets: scala.collection.mutable.ArrayBuffer[Facet]
  ): Unit = {
    removeFacet(pi, pj, facets) match {
      case Some(other) =>
        val pl = other.vertices.find(v => !v.approxEqual(pi) && !v.approxEqual(pj)).get
        insertFacet(pr, pi, pl, base, facets)
        insertFacet(pr, pl, pj, base, facets)
        insertFacet(pr, pj, pk, base, facets)
        insertFacet(pr, pk, pi, base, facets)
        legalizeEdge(pr, pi, pl, base, facets)
        legalizeEdge(pr, pl, pj, base, facets)
        legalizeEdge(pr, pj, pk, base, facets)
        legalizeEdge(pr, pk, pi, base, facets)
      case None =>
        insertFacet(pr, pj, pk, base, facets)
        insertFacet(pr, pk, pi, base, facets)
        legalizeEdge(pr, pj, pk, base, facets)
        legalizeEdge(pr, pk, pi, base, facets)
    }
  }

  def triangulate(base: Facet, points: Seq[Vertex]): Seq[Facet] = {

    val facets = scala.collection.mutable.ArrayBuffer[Facet]()
    facets += base

    // Insert each point.
    points.foreach { p =>

      // Find a triangle containing p and remove it from the set.
      val i = facets.indexWhere(_.contains(p))
      if (i >= 0) {
        val facet = facets(i)
        facets.remove(i)

        // Add p
        if (p.approxEqual(facet.v1) || p.approxEqual(facet.v2) || p.approxEqual(facet.v3)) {
          // p already exists as an edge; nothing to do.
          facets += facet
        } else if (p.collinear(facet.v1, facet.v2)) {
          // p along [v1, v2]
          addCollinear(p, facet.v1, facet.v2, facet.v3, base, facets)
        } else if (p.collinear(facet.v2, facet.v3)) {
          // p along [v2, v3]
          addCollinear(p, facet.v2, facet.v3, facet.v1, base, facets)
        } else if (p.collinear(facet.v3, facet.v1)) {
          // p along [v3, v1]
          addCollinear(p, facet.v3, facet.v1, facet.v2, base, facets)
        } else {
          // pr inside t; add edges from pr to the 3 vertices.
          val (pi, pj, pk) = (facet.v1, facet.v2, facet.v3)
          insertFacet(p, pi, pj, base, facets)
          insertFacet(p, pj, pk, base, facets)
          insertFacet(p, pk, pi, base, facets)
          legalizeEdge(p, pi, pj, facet, facets)
          legalizeEdge(p, pj, pk, base, facets)
          legalizeEdge(p, pk, pi, base, facets)
        }
      }
    }

    facets
  }

}
