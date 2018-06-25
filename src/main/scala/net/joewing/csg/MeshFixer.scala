package net.joewing.csg

case class MeshFixer(facets: Seq[Facet], epsilon: Double = 1e-6) {

  private sealed trait Edge {
    val v1: Vertex
    val v2: Vertex
  }

  private case class ForwardEdge(v1: Vertex, v2: Vertex) extends Edge

  private case class BackwardEdge(v1: Vertex, v2: Vertex) extends Edge

  private case class Face(e1: Edge, e2: Edge, e3: Edge)

  private val vertices = scala.collection.mutable.ArrayBuffer[Vertex]()
  private val forwardEdges = scala.collection.mutable.ArrayBuffer[ForwardEdge]()
  private val backwardEdges = scala.collection.mutable.Map[ForwardEdge, BackwardEdge]()
  private val faces = scala.collection.mutable.ArrayBuffer[Face]()

  private def getVertex(v: Vertex): Vertex = {
    if (vertices.nonEmpty) {
      val closest = vertices.minBy(other => (other - v).length)
      if ((closest - v).length < epsilon) {
        closest
      } else {
        vertices += v
        v
      }
    } else {
      vertices += v
      v
    }
  }

  private def getEdge(v1: Vertex, v2: Vertex): Edge = {
    val updatedV1 = getVertex(v1)
    val updatedV2 = getVertex(v2)
    forwardEdges.find { edge =>
      edge.v1 == updatedV2 && edge.v2 == updatedV1
    } match {
      case Some(fe) =>
        val be = BackwardEdge(updatedV1, updatedV2)
        backwardEdges += fe -> be
        be
      case None =>
        val fe = ForwardEdge(updatedV1, updatedV2)
        forwardEdges += fe
        fe
    }
  }

  private def getFace(facet: Facet): Face = Face(
    getEdge(facet.v1, facet.v2),
    getEdge(facet.v2, facet.v3),
    getEdge(facet.v3, facet.v1)
  )

  private def loadFacets(): Unit = {
    println("Loading facets")
    facets.foreach { facet =>
      faces += getFace(facet)
    }
    println(
      s"... faces: ${faces.size}, " +
      s"forward edges: ${forwardEdges.size}, " +
      s"backward edges: ${backwardEdges.size}, " +
      s"vertices: ${vertices.size}"
    )
  }

  private def reversedEdgeDistance[T <: Edge](e1: T, e2: T): Double = (e1.v1 - e2.v2).length + (e1.v2 - e2.v1).length

  private def combineEdges(): Unit = {
    println("Combining edges")

    var badEdges = scala.collection.mutable.ArrayBuffer(forwardEdges.filterNot(backwardEdges.contains): _*)
    println(s"... ${badEdges.size} edges to fix")
    /*
    while (badEdges.size > 1) {

      val pairs = for {
        i <- badEdges.indices
        j <- (i + 1) until badEdges.size
      } yield (i, j)
      val (feIndex, oldEdgeIndex) = pairs.minBy { case (i, j) => reversedEdgeDistance(badEdges(i), badEdges(j)) }
      val (fe, oldEdge) = (badEdges(feIndex), badEdges(oldEdgeIndex))
      badEdges.remove(feIndex)
      badEdges.remove(oldEdgeIndex)

      val distance = reversedEdgeDistance(fe, oldEdge)
      println(distance)
      if (distance < 1) {
        val newEdge = BackwardEdge(fe.v2, fe.v1)
        backwardEdges += fe -> newEdge
        forwardEdges -= oldEdge

        val i = faces.indexWhere(f => f.e1 == oldEdge || f.e2 == oldEdge || f.e3 == oldEdge)
        val face = faces(i)
        faces(i) = Face(
          if (face.e1 == oldEdge) newEdge else face.e1,
          if (face.e2 == oldEdge) newEdge else face.e2,
          if (face.e3 == oldEdge) newEdge else face.e3
        )
      }
    }
    println(s"... forward edges: ${forwardEdges.size}, backward edges: ${backwardEdges.size}")
    */
  }

  loadFacets()
  combineEdges()

  def fixed: Seq[Facet] = faces.map(f => Facet(f.e1.v1, f.e2.v1, f.e3.v1))

}

