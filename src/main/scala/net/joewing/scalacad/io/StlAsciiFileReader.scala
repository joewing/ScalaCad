package net.joewing.scalacad.io

import java.io.{FileInputStream, InputStream, InputStreamReader}

import net.joewing.scalacad._
import net.joewing.scalacad.primitives.Primitive3d

import scala.util.parsing.combinator.JavaTokenParsers

class StlAsciiFileReader(is: InputStream) {

  private case class Stl(name: String, facets: IndexedSeq[Facet])

  private object StlParser extends JavaTokenParsers {

    val startSolid: Parser[String] = "solid .*".r

    val endSolid: Parser[String] = "endsolid .*".r

    val vertex: Parser[Vertex] = floatingPointNumber ~ floatingPointNumber ~ floatingPointNumber ^^ {
      case x1 ~ x2 ~ x3 => Vertex(x1.toDouble, x2.toDouble, x3.toDouble)
    }

    val startFacet: Parser[Vertex] = "facet normal " ~> vertex

    val endFacet: Parser[String] = "endfacet"

    val startLoop: Parser[String] = "outer loop"

    val endLoop: Parser[String] = "endloop"

    val facetVertex: Parser[Vertex] = "vertex " ~> vertex

    val vertices: Parser[(Vertex, Vertex, Vertex)] = (facetVertex ~ facetVertex ~ facetVertex) ^^ {
      case v1 ~ v2 ~ v3 => (v1, v2, v3)
    }

    val loop: Parser[(Vertex, Vertex, Vertex)] = startLoop ~> vertices <~ endLoop

    val facet: Parser[Facet] = startFacet ~ loop <~ endFacet ^^ {
      case _ ~ vs => Facet(vs._1, vs._2, vs._3)
    }

    val facets: Parser[Seq[Facet]] = facet.*

    val solid: Parser[Stl] = startSolid ~ facets <~ endSolid ^^ {
      case n ~ fs => Stl(n, fs.toVector)
    }

    def parse(is: InputStream): ParseResult[Stl] = parseAll(solid, new InputStreamReader(is))
  }

  def read: Primitive3d = {
    StlParser.parse(is) match {
      case StlParser.Success(stl,_ ) => Primitive3d(stl.facets)
      case StlParser.Error(msg, _)   => throw new IllegalArgumentException(s"parse error: $msg")
      case StlParser.Failure(msg, _) => throw new IllegalArgumentException(s"parse failure: $msg")
    }
  }
}

object StlAsciiFileReader {
  def read(is: InputStream): Primitive3d = new StlAsciiFileReader(is).read
  def read(fileName: String): Primitive3d = read(new FileInputStream(fileName))
}