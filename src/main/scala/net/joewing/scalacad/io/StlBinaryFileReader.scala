package net.joewing.scalacad.io

import java.io.{FileInputStream, InputStream}
import java.nio.{ByteBuffer, ByteOrder}

import net.joewing.scalacad.primitives.Primitive3d
import net.joewing.scalacad.{Facet, Polygon3d, Vertex}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

class StlBinaryFileReader(is: InputStream) {

  @tailrec
  private def loadStream(is: InputStream, buffer: ArrayBuffer[Byte]): Unit = {
    val ch = is.read()
    if (ch >= 0) {
      buffer.append(ch.toByte)
      loadStream(is, buffer)
    } else {
      is.close()
    }
  }

  def readVertex(buffer: ByteBuffer): Vertex = {
    Vertex(buffer.getFloat, buffer.getFloat, buffer.getFloat)
  }

  def readFacet(buffer: ByteBuffer): Facet = {
    readVertex(buffer)  // Normal
    val facet = Facet(
      readVertex(buffer),
      readVertex(buffer),
      readVertex(buffer)
    )
    buffer.getShort()
    facet
  }

  def read: Primitive3d = {
    val array = ArrayBuffer[Byte]()
    loadStream(is, array)

    val buffer = ByteBuffer.wrap(array.toArray)
    buffer.order(ByteOrder.LITTLE_ENDIAN)

    // Read the name
    val name = Array.fill[Byte](80)(0)
    buffer.get(name)

    // Read the number of facets.
    val facetCount = buffer.getInt

    // Read the facets.
    val facets = (0 until facetCount).map { _ =>
      readFacet(buffer)
    }
    Primitive3d(facets.map(f => Polygon3d(f.vertices)))
  }
}

object StlBinaryFileReader {
  def read(is: InputStream): Primitive3d = new StlBinaryFileReader(is).read
  def read(fileName: String): Primitive3d = read(new FileInputStream(fileName))
}
