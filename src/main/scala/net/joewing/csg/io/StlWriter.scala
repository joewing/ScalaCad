package net.joewing.csg.io

import net.joewing.csg.Stl

trait StlWriter {
  def write(stl: Stl): Unit
}
