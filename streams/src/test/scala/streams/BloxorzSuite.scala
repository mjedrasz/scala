package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._
import java.util.regex.Pattern.Pos
import java.util.regex.Pattern.Pos

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) {
        case (block, move) => move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
      }
  }

  trait Level1 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0, 0)), "0,0")
      assert(terrain(Pos(1, 1)), "1,1") // start
      assert(terrain(Pos(4, 7)), "4,7") // goal
      assert(terrain(Pos(5, 8)), "5,8")
      assert(!terrain(Pos(5, 9)), "5,9")
      assert(terrain(Pos(4, 9)), "4,9")
      assert(!terrain(Pos(6, 8)), "6,8")
      assert(!terrain(Pos(4, 11)), "4,11")
      assert(!terrain(Pos(-1, 0)), "-1,0")
      assert(!terrain(Pos(0, -1)), "0,-1")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1, 1))
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      println(solution)
      println(solve(solution))
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

  test("is block standing") {
    new Level1 {
      val block = Block(Pos(0, 0), Pos(0, 0))
      assert(block.isStanding, true)
      assert(!block.left.isStanding, false)
      assert(block.left.right.isStanding, true)
      assert(block.left.left.isStanding, true)
      assert(!block.left.up.isStanding, false)
      assert(!block.left.up.up.isStanding, false)
      assert(block.left.up.up.left.isStanding, true)
      assert(block.left.up.up.right.isStanding, true)
    }
  }

  test("is legal") {
    new Level1 {
      val block = Block(Pos(0, 0), Pos(0, 0))
      assert(block.isLegal, true)
      assert(!block.left.isLegal, false)
      assert(!block.up.isLegal, false)
      assert(block.right.isLegal, true)
      assert(block.down.isLegal, true)
      assert(!block.down.left.isLegal, false)

    }
  }

  test("neighbors") {
    new Level1 {
      val standingBlock = Block(Pos(0, 0), Pos(0, 0))
      assert(standingBlock.neighbors == List(
        (Block(Pos(-2, 0), Pos(-1, 0)), Up),
        (Block(Pos(1, 0), Pos(2, 0)), Down),
        (Block(Pos(0, -2), Pos(0, -1)), Left),
        (Block(Pos(0, 1), Pos(0, 2)), Right)))
      val layingBlock = Block(Pos(0, 0), Pos(0, 1))
      assert(layingBlock.neighbors == List(
        (Block(Pos(-1, 0), Pos(-1, 1)), Up),
        (Block(Pos(1, 0), Pos(1, 1)), Down),
        (Block(Pos(0, -1), Pos(0, -1)), Left),
        (Block(Pos(0, 2), Pos(0, 2)), Right)))

    }
  }

  test("legal neighbors") {
    new Level1 {
      val standingBlock = Block(Pos(0, 0), Pos(0, 0))
      assert(standingBlock.legalNeighbors == List(
        (Block(Pos(1, 0), Pos(2, 0)), Down),
        (Block(Pos(0, 1), Pos(0, 2)), Right)))
      val layingBlock = Block(Pos(0, 0), Pos(0, 1))
      assert(layingBlock.legalNeighbors == List(
        (Block(Pos(1, 0), Pos(1, 1)), Down),
        (Block(Pos(0, 2), Pos(0, 2)), Right)))

    }
  }

  test("done") {
    new Level1 {
      assert(!done(Block(Pos(1, 0), Pos(1, 1))))
      assert(done(Block(Pos(4, 7), Pos(4, 7))))
    }
  }

  test("neighbors with history") {
    new Level1 {
      val block = Block(Pos(4, 7), Pos(4, 7))
      val moves = List(Right, Right, Down, Right, Right, Right, Down)
      val (b, history) = neighborsWithHistory(block, moves).toList.head
      assert(b == Block(Pos(2, 7), Pos(3, 7)) && history.head == Up)
      val t = neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up)).toSet
      assert(t == Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))))
    }
  }

  test("neighbors with history - new only") {
    new Level1 {
      val neighbors = newNeighborsOnly(
        Set(
          (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
          (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream,

        Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1))))
      assert(neighbors.toSet == Set(
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))))
    }

  }

}
