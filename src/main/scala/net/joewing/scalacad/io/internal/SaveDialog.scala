package net.joewing.scalacad.io.internal

import java.nio.file.Paths

import javax.swing._
import javax.swing.filechooser.FileNameExtensionFilter
import net.joewing.scalacad.io.{ObjFileWriter, StlAsciiFileWriter, StlBinaryFileWriter}
import net.joewing.scalacad.primitives.{Primitive, ThreeDimensional}

class SaveDialog(parent: JFrame, obj: Primitive[ThreeDimensional]) {

  private type SaveFunction = (Primitive[ThreeDimensional], String) => Unit

  private case class FileType(
    description: String,
    extension: String,
    saveFunction: SaveFunction
  ) {
    val filter: FileNameExtensionFilter = new FileNameExtensionFilter(description, extension)
  }

  private val chooser = new JFileChooser()

  private val filters: Seq[FileType] = Seq(
    FileType("Binary STL File", "stl", StlBinaryFileWriter.write),
    FileType("ASCII STL File", "stl", StlAsciiFileWriter.write),
    FileType("OBJ File", "obj", ObjFileWriter.write)
  )

  private def confirmSave(fileName: String): Boolean = {
    val response = JOptionPane.showConfirmDialog(
      parent,
      s"$fileName already exists, overwrite?",
      "Save",
      JOptionPane.YES_NO_OPTION,
      JOptionPane.QUESTION_MESSAGE
    )
    response == JOptionPane.YES_OPTION
  }

  private def saveFile(fileName: String, ftype: FileType): Unit = {
    val fixedFileName = if (fileName.toLowerCase.endsWith(s".${ftype.extension}")) {
      fileName
    } else {
      s"$fileName.${ftype.extension}"
    }
    val shouldSave = !Paths.get(fixedFileName).toFile.exists || confirmSave(fixedFileName)
    if (shouldSave) {
      ftype.saveFunction(obj, fixedFileName)
    }
  }

  def show(): Unit = {
    chooser.setCurrentDirectory(Paths.get(".").toFile)
    chooser.getChoosableFileFilters.foreach(chooser.removeChoosableFileFilter)
    filters.foreach(f => chooser.addChoosableFileFilter(f.filter))
    chooser.setFileFilter(filters.head.filter)
    if (chooser.showSaveDialog(parent) == JFileChooser.APPROVE_OPTION) {
      val ftype = filters.find(_.description == chooser.getFileFilter.getDescription).get
      val fileName = chooser.getSelectedFile.getPath
      saveFile(fileName, ftype)
    }
  }
}

object SaveDialog {
  def show(parent: JFrame, obj: Primitive[ThreeDimensional]): Unit = {
    val dialog = new SaveDialog(parent, obj)
    dialog.show()
  }
}
