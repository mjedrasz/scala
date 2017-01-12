package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 1,
    Key.exec.maxWarmupRuns -> 1,
    Key.exec.benchRuns -> 1,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
//    val length = 1000
    val chars = "()).".toArray//new Array[Char](length)
    val threshold = 1
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {

    def innerBalance(chars: Array[Char], stack: Int): Int = {
      if (stack < 0) {
        -1
      } else if (chars.isEmpty) {
        stack
      } else {
        if (chars.head == '(') {
          innerBalance(chars.tail, stack + 1)
        } else if (chars.head == ')') {
          innerBalance(chars.tail, stack - 1)
        } else {
          innerBalance(chars.tail, stack)
        }
      }
    }

    innerBalance(chars, 0) == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, opening: Int, closing: Int): (Int, Int) = {
      if (idx == until) {
        (opening, closing)
      } else {
        if (chars(idx) == '(') {
          traverse(idx + 1, until, opening + 1, closing)
        } else if (chars(idx) == ')') {
          traverse(idx + 1, until, opening, closing + 1)
        } else {
          traverse(idx + 1, until, opening, closing)
        }
      }
    }
           //(())
    ///////((    ))
    //////(  (  )  )
    //0 2 2 4
    //0 1, 1 2
    //2 3, 3 4
    //(1, 0) (1, 0)
    //(0, 1) (0, 1)
    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) {
        traverse(from, until, 0, 0)
      } else {
        val mid = from + ((until - from)/2)
        val ((o1, c1), (o2, c2)) = parallel(reduce(from, mid), reduce(mid, until))
        (o1 - c1 - c2, c2 - o2 - o1)
        
        //(( ))
        //(2, 0) (0, 2)
        //(o1 - c1 - c2, c2 - o2 - o1)
        //(0, 0)
        //)) ((
        //(0, 2) (2 0)
        //(0 - 2 - 0, 0 - 2 - 0) = (-2, -2)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
