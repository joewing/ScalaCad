package net.joewing.csg.io

import java.io.{FileInputStream, InputStream, InputStreamReader}

import net.joewing.csg._

import scala.util.parsing.combinator.JavaTokenParsers

class StlAsciiFileReader(is: InputStream) extends StlReader {

  private object StlParser extends JavaTokenParsers {

    val solidName: Parser[String] = regex(".*".r)

    val startSolid: Parser[String] = "solid " ~> solidName

    val endSolid: Parser[String] = "endsolid " ~> solidName

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
      case n ~ fs => Stl(n, fs)
    }

    def parse(is: InputStream): ParseResult[Stl] = parseAll(solid, new InputStreamReader(is))
  }

  def read: Stl = {
    StlParser.parse(is) match {
      case StlParser.Success(stl,_ ) => stl
      case StlParser.Error(msg, _)   => throw new IllegalArgumentException(s"parse error: $msg")
      case StlParser.Failure(msg, _) => throw new IllegalArgumentException(s"parse failure: $msg")
    }
  }
}

object StlAsciiFileReader {
  def read(is: InputStream): Stl = new StlAsciiFileReader(is).read
  def read(fileName: String): Stl = read(new FileInputStream(fileName))
}