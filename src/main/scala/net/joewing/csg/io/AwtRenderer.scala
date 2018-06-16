package net.joewing.csg.io

import java.awt.event._
import java.awt.image.BufferedImage
import java.awt.{Color, Graphics2D}

import javax.swing.{ImageIcon, JFrame, JLabel, WindowConstants}
import net.joewing.csg._
import net.joewing.csg.primitives.Cylinder
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

  private def paint(bsp: BSPTree, p: Vertex): Seq[Facet] = {
    val front = bsp.front.toSeq.flatMap(t => paint(t, p))
    val back = bsp.back.toSeq.flatMap(t => paint(t, p))
    val dp = p.dot(bsp.plane.normal)
    if (dp > 0) {
      back ++ bsp.facets ++ front
    } else if (dp < 0) {
      front ++ bsp.facets ++ back
    } else {
      front ++ back
    }
  }

  private def orderedPolygons: Seq[(Array[Int], Array[Int])] = {
    val bsp = BSPTree {
      stl.facets.map { facet =>
        facet
          .scaled(scale, scale, scale)
          .rotated(x = rotationY, y = rotationX)
          .moved(x = positionX, y = positionY)
      }
    }
    paint(bsp, Vertex(0, 0, -10)).map { facet =>
      val xs = facet.vertices.map { v =>
        projection.project(v)._1.toInt
      }
      val ys = facet.vertices.map { v =>
        projection.project(v)._2.toInt
      }
      (xs.toArray, ys.toArray)
    }
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
    orderedPolygons.foreach { case (xpoints, ypoints) =>
      renderFacet(xpoints, ypoints, graphics)
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
      if ((e.getModifiersEx & (InputEvent.BUTTON2_DOWN_MASK | InputEvent.CTRL_DOWN_MASK)) == 0) {
        rotationX += math.Pi * dx / 180.0
        rotationY += math.Pi * dy / 180.0
      } else {
        positionX += dx / 2.0
        positionY += dy / 2.0
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
}
