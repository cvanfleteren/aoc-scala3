package be.cvalue.common


/**
 * Coordinate for the Grid.
 * 0,0 is top left, 1,1 is one down and one to the right
 * so down from (0,0) would be (1,0), right from (0,0) would be (0,1)
 */
case class XY(x: Int, y: Int) {

  def neighbours(): List[XY] = {
    List(
      XY(x - 1, y - 1),
      XY(x - 1, y),
      XY(x - 1, y + 1),
      XY(x, y - 1),
      XY(x, y + 1),
      XY(x + 1, y - 1),
      XY(x + 1, y),
      XY(x + 1, y + 1)
    )
  }

  def horAndVerNeighbours(): List[XY] = {
    List(
      XY(x - 1, y),
      XY(x + 1, y),
      XY(x, y - 1),
      XY(x, y + 1)
    )
  }

  def horAndVerNeighboursWithinGrid(grid: Grid[_]): List[XY] = {
    List(
      XY(x - 1, y),
      XY(x + 1, y),
      XY(x, y - 1),
      XY(x, y + 1)
    ).filter(p => p.x > 0 && p.y > 0 && p.x < grid.colSize && p.y  < grid.rowSize)
  }

  def up: XY = XY(x, y -1)

  def down: XY = XY(x, y + 1)

  def left: XY = XY(x -1, y)

  def right: XY = XY(x +1, y)

}
