package net.joewing.scalacad

object Delaunay {

  class FacetTree(val facet: Facet, rootOpt: Option[FacetTree]) {
    var children: Seq[FacetTree] = Seq.empty

    def root: FacetTree = rootOpt.getOrElse(this)

    def replace(facets: Facet*): Unit = {
      require(children.isEmpty)
      children = facets.map(f => new FacetTree(f, Some(this)))
    }

    def find(p: Vertex): Option[FacetTree] = {
      if (facet.contains(p)) {
        if (children.nonEmpty) {
          Some(children.flatMap(c => c.find(p)).head)
        } else Some(this)
      } else None
    }

    def incident(pi: Vertex, pj: Vertex): Option[FacetTree] = {
      if (children.nonEmpty) {
        children.flatMap(_.incident(pi, pj)).headOption
      } else if (facet.edges.exists(e => e._1.approxEqual(pi) && e._2.approxEqual(pj))) {
        Some(this)
      } else None
    }

    def resultTrees: Seq[FacetTree] = {
      if (children.nonEmpty) {
        children.flatMap(_.resultTrees).distinct
      } else Seq(this)
    }

    def results: Seq[Facet] = resultTrees.map(_.facet)
  }

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

  def isLegal(node: FacetTree): Boolean = {
    val (pi, pj) = (node.facet.v2, node.facet.v3)

    // Find the two facets sharing this edge.
    val left = node
    val rightOpt = node.root.incident(pj, pi)

    rightOpt match {
      case Some(right) =>
        // Get the smallest angle of the existing edge.
        val angleExisting = smallestAngle(left.facet, right.facet)

        // Get the smallest angle if we were to flip the edge.
        val (flippedLeft, flippedRight) = flip(left.facet, right.facet, pi, pj)
        val angleFlipped = smallestAngle(flippedLeft, flippedRight)

        // The edge is legal if the smallest angle of the flipped set <= the smallest angle of the existing set
        angleFlipped <= angleExisting

      case _ => true
    }
  }

  def legalizeEdge(node: FacetTree): Unit = {
    if (!isLegal(node)) {
      // Illegal; flip the edge and legalize the new edges.

      val (pr, pi, pj) = (node.facet.v1, node.facet.v2, node.facet.v3)
      val left = node
      val right = node.root.incident(pj, pi).get

      val pk = right.facet.vertices.find(v => !v.approxEqual(pi) && !v.approxEqual(pj)).get

      val t1 = Facet(pr, pk, pj)
      val f1 = if (t1.normal.dot(node.facet.normal) < 0) t1.flip else t1
      val n1 = new FacetTree(f1, Some(node.root))

      val t2 = Facet(pr, pk, pj)
      val f2 = if (t2.normal.dot(node.facet.normal) < 0) t2.flip else t2
      val n2 = new FacetTree(f2, Some(node.root))

      left.children = Seq(n1, n2)
      right.children = Seq(n1, n2)
      legalizeEdge(n1)
      legalizeEdge(n2)
    }
  }

  def addCollinear(
    pr: Vertex,
    pi: Vertex,
    pj: Vertex,
    pk: Vertex,
    node: FacetTree
  ): Unit = {
    node.root.incident(pj, pi) match {
      case Some(other) =>
        val pl = other.facet.vertices.find(v => !v.approxEqual(pi) && !v.approxEqual(pj)).get
        node.replace(Facet(pr, pj, pk), Facet(pr, pk, pi))
        other.replace(Facet(pr, pi, pl), Facet(pr, pl, pj))
        legalizeEdge(node.children(0))
        legalizeEdge(node.children(1))
        legalizeEdge(other.children(0))
        legalizeEdge(other.children(1))
      case None =>
        node.replace(Facet(pr, pj, pk), Facet(pr, pk, pi))
        legalizeEdge(node.children(0))
        legalizeEdge(node.children(1))
    }
  }

  def triangulate(base: Facet, points: Seq[Vertex]): Seq[Facet] = {

    val root = new FacetTree(base, None)

    // Insert each point.
    points.foreach { p =>
      root.find(p).foreach { node =>
        val facet = node.facet
        if (p.approxEqual(facet.v1) || p.approxEqual(facet.v2) || p.approxEqual(facet.v3)) {
          // This point is already contained; skip it.
        } else if (p.collinear(facet.v1, facet.v2)) {
          // p along [v1, v2]
          addCollinear(p, facet.v1, facet.v2, facet.v3, node)
        } else if (p.collinear(facet.v2, facet.v3)) {
          // p along [v2, v3]
          addCollinear(p, facet.v2, facet.v3, facet.v1, node)
        } else if (p.collinear(facet.v3, facet.v1)) {
          // p along [v3, v1]
          addCollinear(p, facet.v3, facet.v1, facet.v2, node)
        } else {
          // pr inside t; add edges from pr to the 3 vertices.
          val (pi, pj, pk) = (facet.v1, facet.v2, facet.v3)
          node.replace(Facet(p, pi, pj), Facet(p, pj, pk), Facet(p, pk, pi))
          legalizeEdge(node.children(0))
          legalizeEdge(node.children(1))
          legalizeEdge(node.children(2))
        }
      }
    }
    root.results
  }

}
