package net.joewing.scalacad.io

import java.awt.event._
import java.awt.image.BufferedImage
import java.awt.{Color, Graphics2D}

import javax.swing._
import net.joewing.scalacad._
import net.joewing.scalacad.io.internal.SaveDialog
import net.joewing.scalacad.primitives.{Primitive, ThreeDimensional}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class AwtRenderer(
  title: String,
  obj: Primitive[ThreeDimensional],
  initialImageWidth: Int,
  initialImageHeight: Int,
  showVertices: Boolean,
  showBackfaces: Boolean
) {

  private val bsp: BSPTree = {
    import scala.concurrent.ExecutionContext.Implicits.global
    Await.result(obj.rendered.treeFuture, Duration.Inf)
  }
  private lazy val minBound: Vertex = obj.minBound
  private lazy val maxBound: Vertex = obj.maxBound

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

  private def render(): Unit = {

    val start = System.currentTimeMillis

    val width = image.getWidth
    val height = image.getHeight
    val graphics = image.createGraphics
    graphics.setColor(Color.BLACK)
    graphics.fillRect(0, 0, width, height)

    val r = math.max(math.max(maxBound.x, maxBound.y), maxBound.z) * 2
    val rx = -rotationX + math.Pi
    val ry = -rotationY + math.Pi
    val lightSource = Vertex(
      r * math.sin(rx) * math.cos(ry),
      r * math.sin(ry),
      r * math.cos(rx) * math.cos(ry)
    ).unit

    val cx = math.cos(rotationX)
    val sx = math.sin(rotationX)
    val cy = math.cos(rotationY)
    val sy = math.sin(rotationY)
    val sycx = sy * cx
    val sysx = sy * sx
    bsp.paint(lightSource, showBackfaces) { polygon =>
      val sz = polygon.vertices.size
      val xs = Array.ofDim[Int](sz)
      val ys = Array.ofDim[Int](sz)
      var xInRange = false
      var yInRange = false
      var i = 0
      while (i < sz) {
        val v = polygon.vertices(i)
        val x = (v.x * cx + v.z * sx) * scale + positionX
        val y = height - (v.x * sysx + v.y * cy - v.z * sycx) * scale - positionY
        xInRange = xInRange || x >= 0 || x < width
        yInRange = yInRange || y >= 0 || y < height
        xs(i) = x.toInt
        ys(i) = y.toInt
        i += 1
      }
      if (xInRange && yInRange && xs.exists(_ != xs.head) && ys.exists(_  != ys.head)) {
        val v = polygon.normal.dot(lightSource)
        val brightness = math.max(0.1, math.min(1.0, v.toDouble)).toFloat
        val color = if (v < 0) Color.RED else new Color(0.0f, 0.0f, brightness)
        graphics.setColor(color)
        graphics.fillPolygon(xs, ys, sz)
        if (showVertices) {
          graphics.setColor(Color.white)
          graphics.drawPolygon(xs, ys, sz)
        }
      }
    }

    val x0 = 50
    val y0 = height - 50

    def drawVector(v: Vertex, c: Color): Unit = {
      graphics.setColor(c)
      val x2 = x0 + v.x * cx + v.z * sx
      val y2 = y0 + v.x * sysx + v.y * cy - v.z * sycx
      graphics.drawLine(x0.toInt, y0.toInt, x2.toInt, y2.toInt)
    }

    drawVector(Vertex(40, 0, 0), Color.red)
    drawVector(Vertex(0, -40, 0), Color.green)
    drawVector(Vertex(0, 0, 40), Color.yellow)

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
        rotationY = math.min(math.max(rotationY, -math.Pi / 2), math.Pi / 2)
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
      if (units != 0) {
        scale *= (if (units > 0) 1.1 else 0.9)
        render()
      }
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
      obj = r.centered,
      initialImageWidth = imageWidth,
      initialImageHeight = imageHeight,
      showVertices = showVertices,
      showBackfaces = showBackfaces
    )
    renderer.show()
  }
}
