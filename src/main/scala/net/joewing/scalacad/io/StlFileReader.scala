package net.joewing.scalacad.io

import java.io.{BufferedInputStream, FileInputStream, InputStream, InputStreamReader}

import net.joewing.scalacad.primitives.ImportedPart

object StlFileReader {

  def isAscii(is: BufferedInputStream): Boolean = {
    val buffer = Array.fill[Byte](5)(0)
    is.mark(5)
    is.read(buffer)
    is.reset()
    buffer.toString == "solid"
  }

  def read(is: InputStream): ImportedPart = {
    val bufferedStream = new BufferedInputStream(is)
    if (isAscii(bufferedStream)) {
      StlAsciiFileReader.read(bufferedStream)
    } else {
      StlBinaryFileReader.read(bufferedStream)
    }
  }

  def read(fileName: String): ImportedPart = read(new FileInputStream(fileName))
}
