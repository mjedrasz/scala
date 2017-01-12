package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }
  
  test("calculator") {
    def expr1: Expr = Literal(10)
    val result = Calculator.computeValues(
        Map("a1" -> Signal(Literal(10)),
            "b2" -> Signal(Plus(Literal(3), Literal(2))),
            "c3" -> Signal(Ref("b2")),
            "d2" -> Signal(Plus(Ref("c3"), Literal(2))),
            "e2" -> Signal(Plus(Ref("c3"), Ref("e2"))),
            "f2" -> Signal(Plus(Ref("f2"), Literal(3)))
            ))
    val a1r:Signal[Double] = result.getOrElse("a1", Signal(Double.NaN))
    val b2r:Signal[Double] = result.getOrElse("b2", Signal(Double.NaN))
    val c2r:Signal[Double] = result.getOrElse("c3", Signal(Double.NaN))
    val d2r:Signal[Double] = result.getOrElse("d2", Signal(Double.NaN))
    val e2r:Signal[Double] = result.getOrElse("e2", Signal(Double.NaN))
    val f2r:Signal[Double] = result.getOrElse("f2", Signal(Double.NaN))
    assert(a1r() == 10)
    assert(b2r() == 5)
    assert(b2r() == 5)
    assert(d2r() == 7)
    assert(e2r() != 3)
     assert(f2r() != 4)
    println(e2r())
     println(f2r())
  }

}
