package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    val bval:Double = b()
    Signal( b()* b() - 4*a()*c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    val deltaVal:Double = delta()
    val aVal = a()
    val bVal = b()
    if (deltaVal < 0) Signal(Set())
    else {
      Signal(Set((-b() +  Math.sqrt(delta()))/(2*a()), (-b() -  Math.sqrt(delta()))/(2*a())))
    }
  }
}
