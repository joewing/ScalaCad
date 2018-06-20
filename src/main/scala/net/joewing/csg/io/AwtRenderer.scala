package net.joewing.csg.io

import java.awt.event._
import java.awt.image.BufferedImage
import java.awt.{Color, Graphics2D}

import javax.swing.{ImageIcon, JFrame, JLabel, WindowConstants}
import net.joewing.csg._
import net.joewing.csg.primitives.{Primitive, ThreeDimensional}
import net.joewing.csg.projection.{OrthographicProjection, Projection}

class AwtRenderer(stl: Stl) {

  private val projection: Projection = OrthographicProjection

  private val imageWidth = 800
  private val imageHeight = 600
  private val (initialScale, initialX, initialY): (Double, Double, Double) = {
    val buffer = (imageWidth * 0.05).toInt
    val (minx, miny) = projection.project(stl.minBound)
    val (maxx, maxy) = projection.project(stl.maxBound)
    val scalex = (imageWidth - buffer) / (maxx - minx)
    val scaley = (imageHeight - buffer) / (maxy - miny)
    val s = math.min(scalex, scaley)
    (s, (imageWidth - (maxx + minx) * s) / 2, (imageHeight - (maxy + miny) * s) / 2)
  }

  private var scale: Double = initialScale
  private var rotationX: Double = 0
  private var rotationY: Double = 0
  private var positionX: Double = initialX
  private var positionY: Double = initialY

  private val frame = new JFrame(stl.name)
  private val image = new BufferedImage(imageWidth, imageHeight, BufferedImage.TYPE_INT_RGB)
  private val label = new JLabel(new ImageIcon(image))

  private def paint(bsp: BSPTree, p: Vertex)(f: Facet => Unit): Unit = {
    val dp = p.dot(bsp.plane.normal)
    if (dp > 0) {
      bsp.back.foreach { x => paint(x, p)(f) }
      bsp.facets.foreach(f)
      bsp.front.foreach { x => paint(x, p)(f) }
    } else if (dp < 0) {
      bsp.front.foreach { x => paint(x, p)(f) }
      bsp.facets.foreach(f)
      bsp.back.foreach { x => paint(x, p)(f) }
    } else {
      bsp.front.foreach { x => paint(x, p)(f) }
      bsp.back.foreach { x => paint(x, p)(f) }
    }
  }

  private lazy val allFacets: Seq[Facet] = stl.facets
  private lazy val bsp: BSPTree = BSPTree(allFacets)
  private lazy val maxBound: Vertex = allFacets.map(_.maxBound).foldLeft(Vertex(0, 0, 0)) { (m, v) =>
    m.copy(
      x1 = math.max(m.x1, v.x1),
      x2 = math.max(m.x2, v.x2),
      x3 = math.max(m.x3, v.x3)
    )
  }

  private def renderFacet(xpoints: Array[Int], ypoints: Array[Int], graphics: Graphics2D): Unit = {
    graphics.setColor(Color.BLUE)
    graphics.fillPolygon(xpoints, ypoints, xpoints.length)
    graphics.setColor(Color.WHITE)
    graphics.drawPolygon(xpoints, ypoints, xpoints.length)
  }

  private def render(): Unit = {
    val graphics = image.createGraphics
    graphics.setColor(Color.BLACK)
    graphics.fillRect(0, 0, image.getWidth, image.getHeight)

    val r = math.max(math.max(maxBound.x1, maxBound.x2), maxBound.x3) * 2

    val rx = -rotationX + math.Pi
    val ry = -rotationY + math.Pi
    val p = Vertex(
      r * math.sin(rx) * math.cos(ry),
      r * math.sin(ry),
      r * math.cos(rx) * math.cos(ry)
    )
    val xs = Array.fill[Int](3)(0)
    val ys = Array.fill[Int](3)(0)
    val pos = Vertex(positionX, positionY, 0)
    paint(bsp, p) { facet =>
      facet.vertices.indices.foreach { i =>
        val v = facet.vertices(i).rotated(x = rotationY, y = rotationX) * scale + pos
        val projected = projection.project(v)
        xs(i) = projected._1.toInt
        ys(i) = projected._2.toInt
      }
      renderFacet(xs, ys, graphics)
    }

    frame.repaint()
  }

  private object MotionListener extends MouseMotionListener {

    private var lastx: Int = 0
    private var lasty: Int = 0

    override def mouseMoved(e: MouseEvent): Unit = {
      lastx = e.getX
      lasty = e.getY
    }

    override def mouseDragged(e: MouseEvent): Unit = {
      val dx = e.getX - lastx
      val dy = e.getY - lasty
      lastx = e.getX
      lasty = e.getY
      if ((e.getModifiersEx & (InputEvent.BUTTON3_DOWN_MASK | InputEvent.CTRL_DOWN_MASK)) == 0) {
        rotationX += math.Pi * dx / 180.0
        rotationY += math.Pi * dy / 180.0
      } else {
        positionX += dx
        positionY += dy
      }
      render()
    }
  }

  private object WheelListener extends MouseWheelListener {
    def mouseWheelMoved(e: MouseWheelEvent): Unit = {
      val units = e.getUnitsToScroll
      if (units > 0) {
        scale *= 1.1
      } else {
        scale *= 0.9
      }
      render()
    }
  }

  def show(): Unit = {
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.add(label)
    frame.addMouseMotionListener(MotionListener)
    frame.addMouseWheelListener(WheelListener)
    frame.pack()
    frame.setVisible(true)
    render()
  }
}

object AwtRenderer {
  def show(stl: Stl): Unit = new AwtRenderer(stl).show()
  def show(r: Primitive[ThreeDimensional]): Unit = show(Stl("ScalaCad", r.render.allFacets))
}
