package spec.gameOfLife.cli

import java.io.FileNotFoundException

import spec.gameOfLife._
/**
 * Created by Charlie Martell on 3/27/15.
 */

object CLI extends App {

  println("Run Game of Life Simulation: Beacon, Blinker, Diehard, Gliders, Pulsar, Gospel-Glider-Gun")
  var choice = readLine().toLowerCase
  var contents: String = null
  try {
    contents = io.Source.fromFile(s"patterns/$choice").mkString
  } catch {
    case e: FileNotFoundException => throw new Exception(s"Error no such game of life associated with $choice")
  }
  val (lifeGrid, gameOfLifeGrid) = fromString(contents)
  nextLife(lifeGrid)

  //Yields next iteration of life
  def nextLife(lifeGrid: LifeGrid): LifeGrid = {
    Thread.sleep(100)
    clearScreen()
    printGameOfLife(lifeGrid, gameOfLifeGrid) foreach println
    val next = gameOfLifeGrid.generateWorld(lifeGrid)
    if (next == lifeGrid) next
    else nextLife(next)
  }

  //Parsers file to generate a Game of Life
  def fromString(input: String): (LifeGrid, GameOfLifeGrid) = {
    val lines = input.split("\\r?\\n")
      .dropWhile(!_.contains('+'))
      .toList
    if (lines.isEmpty) throw new IllegalArgumentException("Input string did not contain starting + marker")
    val content = lines.map(_.take(lines.map(_.drop(lines.head.indexWhere('+' ==))).head.drop(1).indexWhere('+' ==) + 1)) match {
      case startRow :: tail => startRow :: tail.takeWhile(_.head != '+')
      case Nil => Nil
    }
    (content.drop(1)
      .zipWithIndex.flatMap { case ((line, y)) =>
      line.drop(1)
        .zipWithIndex.flatMap { case ((char, x)) =>
        if (char != ' ') Some(Cell(x, y) -> true)
        else None
      }
    }.toMap, GOLGrid(content.head.size - 1, content.size - 1))
  }

  //Prints the Game of Life to Shell
  def printGameOfLife(lifeGrid: LifeGrid, gameOfLifeGrid: GameOfLifeGrid): List[String] = {
    val content = (gameOfLifeGrid.minY(lifeGrid) to gameOfLifeGrid.maxY(lifeGrid)).toList map { y =>
      (gameOfLifeGrid.minX(lifeGrid) to gameOfLifeGrid.maxX(lifeGrid)) map { x =>
        if (lifeGrid.getOrElse(Cell(x,y), false)) 'x'
        else ' '
      }
    }
    val headerFooter = "+" + ("=" * content.head.size) + "+"
    headerFooter ::
      content.map("|" + _.mkString + "|") ::: List(headerFooter)
  }

  //Clears screen in between iterations of Game of Life
  def clearScreen() = print("\033[H\033[2J")
}
