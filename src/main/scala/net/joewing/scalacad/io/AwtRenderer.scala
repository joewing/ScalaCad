package net.joewing.scalacad.io

import java.awt.event._
import java.awt.image.BufferedImage
import java.awt.{Color, Graphics2D}

import javax.swing._
import net.joewing.scalacad._
import net.joewing.scalacad.io.internal.SaveDialog
import net.joewing.scalacad.primitives.{Primitive, ThreeDimensional}

class AwtRenderer(
  title: String,
  obj: Primitive[ThreeDimensional],
  initialImageWidth: Int,
  initialImageHeight: Int,
  showVertices: Boolean,
  showBackfaces: Boolean
) {

  private val offset = (obj.maxBound + obj.minBound) / -2
  private val centered = obj.render.translate(offset)
  private val bsp = centered.tree
  private lazy val polygons: Seq[Polygon3d] = bsp.allPolygons

  private lazy val minBound: Vertex = polygons.flatMap(_.vertices).foldLeft(Vertex.max) { (m, v) =>
    Vertex(
      x = math.min(m.x, v.x),
      y = math.min(m.y, v.y),
      z = math.min(m.z, v.z)
    )
  }

  private lazy val maxBound: Vertex = polygons.flatMap(_.vertices).foldLeft(Vertex.min) { (m, v) =>
    Vertex(
      x = math.max(m.x, v.x),
      y = math.max(m.y, v.y),
      z = math.max(m.z, v.z)
    )
  }

  private lazy val (initialScale, initialX, initialY): (Double, Double, Double) = {
    val buffer = (initialImageWidth * 0.05).toInt
    val (minx, miny) = (minBound.x, minBound.y)
    val (maxx, maxy) = (maxBound.x, maxBound.y)
    val scalex = (initialImageWidth - buffer) / (maxx - minx)
    val scaley = (initialImageHeight - buffer) / (maxy - miny)
    val s = math.min(scalex, scaley)
    (s, (initialImageWidth - (maxx + minx) * s) / 2, (initialImageHeight - (maxy + miny) * s) / 2)
  }

  private def createImage(width: Int, height: Int): BufferedImage = {
    new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
  }

  private var scale: Double = initialScale
  private var rotationX: Double = 0
  private var rotationY: Double = 0
  private var positionX: Double = initialX
  private var positionY: Double = initialY

  private val frame = new JFrame(title)
  private var image = createImage(initialImageWidth, initialImageHeight)
  private val label = new JLabel(new ImageIcon(image))

  private def renderFacet(
    xpoints: Array[Int],
    ypoints: Array[Int],
    color: Color,
    graphics: Graphics2D
  ): Unit = {
    graphics.setColor(color)
    graphics.fillPolygon(xpoints, ypoints, xpoints.length)
    if (showVertices) {
      graphics.setColor(Color.white)
      graphics.drawPolygon(xpoints, ypoints, xpoints.length)
    }
  }

  private def render(): Unit = {

    val start = System.currentTimeMillis

    val graphics = image.createGraphics
    graphics.setColor(Color.BLACK)
    graphics.fillRect(0, 0, image.getWidth, image.getHeight)

    val r = math.max(math.max(maxBound.x, maxBound.y), maxBound.z) * 2

    val rx = -rotationX + math.Pi
    val ry = -rotationY + math.Pi
    val p = Vertex(
      r * math.sin(rx) * math.cos(ry),
      r * math.sin(ry),
      r * math.cos(rx) * math.cos(ry)
    )
    val lightSource = p.unit

    val cx = math.cos(rotationX)
    val sx = math.sin(rotationX)
    val cy = math.cos(rotationY)
    val sy = math.sin(rotationY)
    val sycx = sy * cx
    val sysx = sy * sx
    bsp.paint(lightSource, showBackfaces) { polygon =>
      val sz = polygon.vertices.size
      val xs = Array.fill[Int](sz)(0)
      val ys = Array.fill[Int](sz)(0)
      polygon.vertices.indices.foreach { i =>
        val v = polygon.vertices(i)
        val x = v.x * cx + v.z * sx
        val y = v.x * sysx + v.y * cy - v.z * sycx
        xs(i) = (x * scale + positionX).toInt
        ys(i) = image.getHeight - (y * scale + positionY).toInt
      }
      val v = polygon.normal.dot(lightSource)
      val brightness = math.max(0.1, math.min(1.0, v)).toFloat
      val color = if (v < 0) Color.RED else new Color(0.0f, 0.0f, brightness)
      renderFacet(xs, ys, color, graphics)
    }

    frame.repaint()

    val end = System.currentTimeMillis
    println(s"Render time: ${end - start} ms")
  }

  private def center(): Unit = {
    scale = initialScale
    rotationX = 0
    rotationY = 0
    positionX = initialX
    positionY = initialY
    render()
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
        positionY -= dy
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

  private object ResizeListener extends ComponentAdapter {
    override def componentResized(e: ComponentEvent): Unit = {
      val newWidth = e.getComponent.getWidth
      val newHeight = e.getComponent.getHeight
      image = createImage(newWidth, newHeight)
      label.setIcon(new ImageIcon(image))
      render()
    }
  }

  private object KeyListener extends KeyAdapter {
    override def keyPressed(e: KeyEvent): Unit = {
      e.getKeyChar match {
        case 'c' | 'C' => center()
        case 'q' | 'Q' => frame.dispose()
        case 's' | 'S' => SaveDialog.show(frame, obj)
        case _         => ()
      }
    }
  }

  private def fileMenu: JMenu = {
    val menu = new JMenu("File")

    val saveItem = new JMenuItem("Save (s)")
    saveItem.addActionListener((e: ActionEvent) => SaveDialog.show(frame, obj))
    menu.add(saveItem)

    menu.add(new JSeparator())

    val exitItem = new JMenuItem("Exit (q)")
    exitItem.addActionListener((e: ActionEvent) => frame.dispose())
    menu.add(exitItem)

    menu
  }

  private def viewMenu: JMenu = {
    val menu = new JMenu("View")

    val centerItem = new JMenuItem("Center (c)")
    centerItem.addActionListener((e: ActionEvent) => center())
    menu.add(centerItem)

    menu
  }

  private def menuBar: JMenuBar = {
    val bar = new JMenuBar()
    bar.add(fileMenu)
    bar.add(viewMenu)
    bar
  }

  def show(): Unit = {
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.setJMenuBar(menuBar)
    frame.add(label)
    frame.addMouseMotionListener(MotionListener)
    frame.addMouseWheelListener(WheelListener)
    frame.addComponentListener(ResizeListener)
    frame.addKeyListener(KeyListener)
    frame.pack()
    frame.setVisible(true)
    render()
  }
}

object AwtRenderer {

  val title: String = "ScalaCad"
  val defaultWidth: Int = 800
  val defaultHeight: Int = 600

  def show(
    r: Primitive[ThreeDimensional],
    imageWidth: Int = defaultWidth,
    imageHeight: Int = defaultHeight,
    showBackfaces: Boolean = false,
    showVertices: Boolean = false
  ): Unit = {
    val renderer = new AwtRenderer(
      title = title,
      obj = r,
      initialImageWidth = imageWidth,
      initialImageHeight = imageHeight,
      showVertices = showVertices,
      showBackfaces = showBackfaces
    )
    renderer.show()
  }
}
