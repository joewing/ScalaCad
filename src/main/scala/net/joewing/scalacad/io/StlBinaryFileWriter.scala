package net.joewing.scalacad.io

import java.io.{FileOutputStream, OutputStream}
import java.nio.{ByteBuffer, ByteOrder}

import net.joewing.scalacad.{Facet, Vertex}
import net.joewing.scalacad.primitives.{Primitive, ThreeDimensional}

class StlBinaryFileWriter(os: OutputStream) {

  def writeVertex(buffer: ByteBuffer, vertex: Vertex): Unit = {
    buffer.putFloat(vertex.x.toFloat)
    buffer.putFloat(vertex.y.toFloat)
    buffer.putFloat(vertex.z.toFloat)
  }

  def write(name: String, facets: Seq[Facet]): Unit = {
    try {
      val capacity = math.max(80 + 4, 4 * 3 * 4 + 2)
      val buffer = ByteBuffer.allocate(capacity)
      buffer.order(ByteOrder.LITTLE_ENDIAN)

      // Write the header.
      val nameBytes = name.getBytes
      val nameArray = Array.tabulate[Byte](80) { i =>
        if (i < nameBytes.length) nameBytes(i) else 0
      }
      buffer.put(nameArray)
      buffer.putInt(facets.size)
      os.write(buffer.array, 0, buffer.position)

      // Write facets.
      facets.foreach { facet =>
        buffer.rewind()
        writeVertex(buffer, facet.normal)
        writeVertex(buffer, facet.v1)
        writeVertex(buffer, facet.v2)
        writeVertex(buffer, facet.v3)
        buffer.putShort(0)
        os.write(buffer.array, 0, buffer.position)
      }
    } finally {
      os.close()
    }
  }
}

object StlBinaryFileWriter {
  def write(r: Primitive[ThreeDimensional], os: OutputStream): Unit = {
    new StlBinaryFileWriter(os).write("", r.render.facets)
  }
  def write(r: Primitive[ThreeDimensional], fileName: String): Unit = write(r, new FileOutputStream(fileName))
}
