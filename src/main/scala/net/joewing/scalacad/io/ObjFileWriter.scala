package net.joewing.scalacad.io

import java.io.{FileOutputStream, OutputStream, PrintWriter}

import net.joewing.scalacad.{Facet, Vertex}
import net.joewing.scalacad.primitives.{Primitive, ThreeDimensional}

class ObjFileWriter(os: OutputStream) {

  private def fmt(d: Double): String = f"$d%.6f"

  def write(facets: Seq[Facet]): Unit = {
    val pw = new PrintWriter(os)
    try {
      val vertices = facets.flatMap(_.vertices).foldLeft(Map.empty[Vertex, Int]) { (vs, v) =>
        if (vs.contains(v)) vs else vs.updated(v, vs.size + 1)
      }

      // Write vertices.
      vertices.toSeq.sortBy(_._2).foreach { case (v, _) =>
        pw.println(s"v ${fmt(v.x)} ${fmt(v.y)} ${fmt(v.z)}")
      }

      // Write faces.
      facets.foreach { facet =>
        val v1 = vertices(facet.v1)
        val v2 = vertices(facet.v2)
        val v3 = vertices(facet.v3)
        pw.println(s"f $v1 $v2 $v3")
      }
    } finally {
      pw.close()
    }
  }
}

object ObjFileWriter {
  def write(r: Primitive[ThreeDimensional], os: OutputStream): Unit = {
    new ObjFileWriter(os).write(r.rendered.facets)
  }
  def write(r: Primitive[ThreeDimensional], fileName: String): Unit = {
    write(r, new FileOutputStream(fileName))
  }
}
