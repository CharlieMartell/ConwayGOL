package spec.gameOfLife
/**
 * Created by mac on 3/27/15.
 */

case class Cell(x: Int, y: Int)

trait GridDefintiion{
  //Define Variables of Grid Definition
  val min: Cell
  val max: Cell

  def minX(lifeGrid: LifeGrid): Int = min.x
  def maxX(lifeGrid: LifeGrid): Int = max.x
  def minY(lifeGrid: LifeGrid): Int = min.y
  def maxY(lifeGrid: LifeGrid): Int = max.y

  //Set boundaries
  def setXMin(n: Int): Int = n max min.x
  def setXMax(n: Int): Int = n min max.x
  def setYMin(n: Int): Int = n max min.y
  def setYMax(n: Int): Int = n min max.y
}

trait GameOfLifeGrid extends GridDefintiion{

  def generateWorld(lifeGrid: LifeGrid): LifeGrid = {
    lifeGrid.flatMap {
      case (cell, true) => checkNeighbors(cell)
      case _ => Nil
    }.flatMap {
      case cell => checkNeighbors(cell)
    }.flatMap { neighbor =>
      if (checkLiveNeighbors(neighbor, lifeGrid)) Some(neighbor -> true)
      else None
    }.toMap
  }

  def checkNeighbors(cell: Cell): Set[Cell] = {
    (setXMin(cell.x - 1) to setXMax(cell.x + 1)).flatMap { x =>
      (setYMin(cell.y - 1) to setYMax(cell.y + 1)).flatMap { y =>
        if (x == cell.x && y == cell.y) None
        else Some(Cell(x, y))
      }
    }.toSet
  }

  def checkLiveNeighbors(cell: Cell, lifeGrid: LifeGrid): Boolean = {
    checkNeighbors(cell).count(neighbor => lifeGrid.getOrElse(neighbor, false)) match {
      case x if x < 2 => false
      case 2 => lifeGrid.getOrElse(cell, false)
      case 3 => true
      case x if x > 3 => false
    }
  }
}

class GOLGrid private(minCell: Cell, maxCell: Cell) extends GameOfLifeGrid with GridDefintiion{
  override val min = minCell
  override val max = maxCell
}

object GOLGrid {
  def apply(width: Int, height: Int) = new GOLGrid(Cell(0, 0), Cell(width - 1, height - 1))
}