package net.joewing.scalacad.io

import java.io.{BufferedInputStream, FileInputStream, InputStream}

import net.joewing.scalacad.primitives.Primitive3d

object StlFileReader {

  def isAscii(is: BufferedInputStream): Boolean = {
    val buffer = Array.fill[Byte](5)(0)
    is.mark(5)
    is.read(buffer)
    is.reset()
    new String(buffer) == "solid"
  }

  def read(is: InputStream): Primitive3d = {
    val bufferedStream = new BufferedInputStream(is)
    if (isAscii(bufferedStream)) {
      StlAsciiFileReader.read(bufferedStream)
    } else {
      StlBinaryFileReader.read(bufferedStream)
    }
  }

  def read(fileName: String): Primitive3d = read(new FileInputStream(fileName))
}
