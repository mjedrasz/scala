import common._
import barneshut.conctrees._
import scala.collection.immutable.Stream.Cons

package object barneshut {

  class Boundaries {
    var minX = Float.MaxValue

    var minY = Float.MaxValue

    var maxX = Float.MinValue

    var maxY = Float.MinValue

    def width = maxX - minX

    def height = maxY - minY

    def size = math.max(width, height)

    def centerX = minX + width / 2

    def centerY = minY + height / 2

    override def toString = s"Boundaries($minX, $minY, $maxX, $maxY)"
  }

  sealed abstract class Quad {
    def massX: Float

    def massY: Float

    def mass: Float

    def centerX: Float

    def centerY: Float

    def size: Float

    def total: Int

    def insert(b: Body): Quad
  }

  case class Empty(centerX: Float, centerY: Float, size: Float) extends Quad {
    def massX: Float = centerX
    def massY: Float = centerY
    def mass: Float = 0
    def total: Int = 0
    def insert(b: Body): Quad = Leaf(centerX, centerY, size, List(b))
  }

  case class Fork(
      nw: Quad, ne: Quad, sw: Quad, se: Quad) extends Quad {
    val centerX: Float = (ne.centerX + nw.centerX) / 2
    val centerY: Float = (ne.centerY + se.centerY) / 2
    val size: Float = nw.size
    val mass: Float = nw.mass + ne.mass + sw.mass + se.mass
    val massX: Float = if (mass == 0) centerX else (sw.mass * sw.massX + se.mass * se.massX + nw.mass * nw.massX + ne.mass * ne.massX) / mass
    val massY: Float = if (mass == 0) centerY else (sw.mass * sw.massY + se.mass * se.massY + nw.mass * nw.massY + ne.mass * ne.massY) / mass
    val total: Int = nw.total + ne.total + sw.total + se.total

    def insert(b: Body): Fork = {
      if (b.x < centerX && b.y < centerY) Fork(nw.insert(b), ne, sw, se)
      else if (b.x >= centerX && b.y < centerY) Fork(nw, ne.insert(b), sw, se)
      else if (b.x < centerX && b.y >= centerY) Fork(nw, ne, sw.insert(b), se)
      else Fork(nw, ne, sw, se.insert(b))
    }
  }
//[Test Description] 'insert' should work correctly on a leaf with center (1,1) and size 2
//[Observed Error] Fork(Leaf(0.5,0.5,1.0,List(barneshut.package$Body@481a996b)),
  //Leaf(1.5,0.5,1.0,List(barneshut.package$Body@3d51f06e)),
  //Empty(0.5,1.5,1.0),Empty(1.5,1.5,2.0)) did not equal 
  //Fork(Leaf(0.5,0.5,1.0,List(barneshut.package$Body@481a996b)),
  //Leaf(1.5,0.5,1.0,List(barneshut.package$Body@3d51f06e)),
  //Empty(0.5,1.5,1.0),Empty(1.5,1.5,1.0)) 
  //expected Fork(Leaf(0.5,0.5,1.0,List(barneshut.package$Body@481a996b)),
  //Leaf(1.5,0.5,1.0,List(barneshut.package$Body@3d51f06e)),Empty(0.5,1.5,1.0),Empty(1.5,1.5,1.0))
  //found Fork(Leaf(0.5,0.5,1.0,List(barneshut.package$Body@481a996b)),
  //Leaf(1.5,0.5,1.0,List(barneshut.package$Body@3d51f06e)),Empty(0.5,1.5,1.0),Empty(1.5,1.5,2.0))

  case class Leaf(centerX: Float, centerY: Float, size: Float, bodies: Seq[Body])
      extends Quad {
    val (mass, massX, massY) = (bodies.foldRight(0f)(_.mass + _): Float,
      bodies.foldRight(0f)((a, b) => a.mass * a.x + b) / bodies.foldRight(0f)(_.mass + _): Float,
      bodies.foldRight(0f)((a, b) => a.mass * a.y + b) / bodies.foldRight(0f)(_.mass + _): Float)
    val total: Int = bodies.size
    def insert(b: Body): Quad = {
      if (size > minimumSize) {
        val f = Fork(Empty(centerX / 2, centerY / 2, size / 2), Empty(centerX + centerX / 2, centerY / 2, size / 2),
          Empty(centerX / 2, centerY + centerY / 2, size / 2), Empty(centerX + centerX / 2, centerY + centerY / 2, size/2))
        val allBodies = bodies :+ b
        allBodies.foldRight(f)((bb, f) => f.insert(bb))
      } else Leaf(centerX, centerY, size, bodies :+ b)
    }
  }

  def minimumSize = 0.00001f

  def gee: Float = 100.0f

  def delta: Float = 0.01f

  def theta = 0.5f

  def eliminationThreshold = 0.5f

  def force(m1: Float, m2: Float, dist: Float): Float = gee * m1 * m2 / (dist * dist)

  def distance(x0: Float, y0: Float, x1: Float, y1: Float): Float = {
    math.sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)).toFloat
  }

  class Body(val mass: Float, val x: Float, val y: Float, val xspeed: Float, val yspeed: Float) {

    def updated(quad: Quad): Body = {
      var netforcex = 0.0f
      var netforcey = 0.0f

      def addForce(thatMass: Float, thatMassX: Float, thatMassY: Float): Unit = {
        val dist = distance(thatMassX, thatMassY, x, y)
        /* If the distance is smaller than 1f, we enter the realm of close
         * body interactions. Since we do not model them in this simplistic
         * implementation, bodies at extreme proximities get a huge acceleration,
         * and are catapulted from each other's gravitational pull at extreme
         * velocities (something like this:
         * http://en.wikipedia.org/wiki/Interplanetary_spaceflight#Gravitational_slingshot).
         * To decrease the effect of this gravitational slingshot, as a very
         * simple approximation, we ignore gravity at extreme proximities.
         */
        if (dist > 1f) {
          val dforce = force(mass, thatMass, dist)
          val xn = (thatMassX - x) / dist
          val yn = (thatMassY - y) / dist
          val dforcex = dforce * xn
          val dforcey = dforce * yn
          netforcex += dforcex
          netforcey += dforcey
        }
      }

      def traverse(quad: Quad): Unit = (quad: Quad) match {
        case Empty(_, _, _) =>
        // no force
        case Leaf(_, _, _, bodies) =>
          bodies.foreach { b => addForce(b.mass, b.x, b.y) }
        // add force contribution of each body by calling addForce
        case f @ Fork(nw, ne, sw, se) =>
          if (f.size / distance(f.massX, f.massY, x, y) < theta) {
            addForce(f.mass, f.massX, f.massY)
          } else {
            traverse(nw)
            traverse(ne)
            traverse(sw)
            traverse(se)
          }
        // see if node is far enough from the body,
        // or recursion is needed
      }

      traverse(quad)

      val nx = x + xspeed * delta
      val ny = y + yspeed * delta
      val nxspeed = xspeed + netforcex / mass * delta
      val nyspeed = yspeed + netforcey / mass * delta

      new Body(mass, nx, ny, nxspeed, nyspeed)
    }

  }

  val SECTOR_PRECISION = 8

  class SectorMatrix(val boundaries: Boundaries, val sectorPrecision: Int) {
    val sectorSize = boundaries.size / sectorPrecision
    val matrix = new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)
    for (i <- 0 until matrix.length) matrix(i) = new ConcBuffer

    /**
     * This method should use the body position, boundaries and sectorPrecision to determine the sector into
     *  which the body should go into, and add the body into the corresponding ConcBuffer object.
     *
     * Importantly, if the Body lies outside of the Boundaries, it should be considered to be located at the closest
     * point within the Boundaries for the purpose of finding which ConcBuffer should hold the body.
     */
    def +=(b: Body): SectorMatrix = {
      val x: Int = Math.min(((b.x - boundaries.minX) / sectorSize).intValue(), sectorPrecision)
      val y: Int = Math.min(((b.y - boundaries.minY) / sectorSize).intValue(), sectorPrecision)
      matrix(Math.min(y * sectorPrecision + x, (sectorPrecision * sectorPrecision) -1)) += b
      this
    }

    def apply(x: Int, y: Int) = matrix(y * sectorPrecision + x)

    def combine(that: SectorMatrix): SectorMatrix = {
      for (i <- 0 until matrix.length) matrix(i) = matrix(i).combine(that.matrix(i))
      this
    }

    def toQuad(parallelism: Int): Quad = {
      def BALANCING_FACTOR = 4
      def quad(x: Int, y: Int, span: Int, achievedParallelism: Int): Quad = {
        if (span == 1) {
          val sectorSize = boundaries.size / sectorPrecision
          val centerX = boundaries.minX + x * sectorSize + sectorSize / 2
          val centerY = boundaries.minY + y * sectorSize + sectorSize / 2
          var emptyQuad: Quad = Empty(centerX, centerY, sectorSize)
          val sectorBodies = this(x, y)
          sectorBodies.foldLeft(emptyQuad)(_ insert _)
        } else {
          val nspan = span / 2
          val nAchievedParallelism = achievedParallelism * 4
          val (nw, ne, sw, se) =
            if (parallelism > 1 && achievedParallelism < parallelism * BALANCING_FACTOR) parallel(
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism))
            else (
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism))
          Fork(nw, ne, sw, se)
        }
      }

      quad(0, 0, sectorPrecision, 1)
    }

    override def toString = s"SectorMatrix(#bodies: ${matrix.map(_.size).sum})"
  }

  class TimeStatistics {
    private val timeMap = collection.mutable.Map[String, (Double, Int)]()

    def clear() = timeMap.clear()

    def timed[T](title: String)(body: => T): T = {
      var res: T = null.asInstanceOf[T]
      val totalTime = /*measure*/ {
        val startTime = System.currentTimeMillis()
        res = body
        (System.currentTimeMillis() - startTime)
      }

      timeMap.get(title) match {
        case Some((total, num)) => timeMap(title) = (total + totalTime, num + 1)
        case None => timeMap(title) = (0.0, 0)
      }

      println(s"$title: ${totalTime} ms; avg: ${timeMap(title)._1 / timeMap(title)._2}")
      res
    }

    override def toString = {
      timeMap map {
        case (k, (total, num)) => k + ": " + (total / num * 100).toInt / 100.0 + " ms"
      } mkString ("\n")
    }
  }
}
