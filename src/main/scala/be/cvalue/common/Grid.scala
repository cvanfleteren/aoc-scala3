package be.cvalue.common


case class Grid[T](private val _rows: Vector[Vector[T]]) {

  def map[A](f: T => A): Grid[A] = {
    Grid(_rows.map(_.map(f)))
  }

  def row(index:Int): List[T] = {
    _rows(index).toList
  }

  def column(index:Int): List[T] = {
    _rows.toList.map(r => r(index))
  }

  private lazy val _columns = {
    val res = 0.to(row(0).length - 1).map { columnIndex =>
      column(columnIndex)
    }
    res.toList
  }

  lazy val rowSize = rows().size
  lazy val colSize = columns().size

  def columns() : List[List[T]] = {
    _columns
  }

  def rows(): List[List[T]] = {
    _rows.toList.map(_.toList)
  }

  def cellAt(coord: XY): T = {
    _rows(coord.y)(coord.x)
  }

  def maybeCellAt(coord: XY): Option[T] = {
    if (coord.y >= 0 && coord.y < _rows.length && coord.x >= 0 && coord.x < _rows(0).length) {
      Some(_rows(coord.y)(coord.x))
    } else {
      None
    }
  }

  def adjacentCells(coord: XY): List[T] = {
    coord.neighbours().flatMap(maybeCellAt)
  }

  def adjacentHorAndVerCells(coord:XY): List[T] = {
    coord.horAndVerNeighbours().flatMap(maybeCellAt)
  }

  def existingCoords(in: List[XY]) : List[XY] = {
    in.filter(coord => maybeCellAt(coord).isDefined)
  }

  def allCells(): List[(XY, T)] = {
    _rows.zipWithIndex.flatMap(row => row._1.zipWithIndex.map(cell => (XY(cell._2, row._2), cell._1))).toList
  }

  def withValue(coord: XY, t: T): Grid[T] = {
    Grid(_rows.updated(coord.y, _rows(coord.y).updated(coord.x,t)))
  }

  def rowOf(coord: XY): List[T] = {
    _rows(coord.y).toList
  }

  def withXY: Grid[(XY, T)] = {
    val newRows = _rows.zipWithIndex.map { (row, x) =>
      row.zipWithIndex.map { (cell, y) => (XY(x, y), cell) }
    }
    Grid(newRows)
  }

  def rotatedCCW : Grid[T] = {
    Grid.fromRows(this.rows().transpose.reverse)
  }

  def rotatedCW : Grid[T] = {
    Grid.fromRows(this.rows().reverse.transpose)
  }


  def walkingDistance(from:XY, to:XY): Int = {
    Math.abs(from.x - to.x) + Math.abs(from.y - to.y)
  }

  override def toString: String = {
    rows().map(row => row.mkString("")).mkString("\n")
  }
}

object Grid {

  def fromRows[T](in: List[List[T]]) : Grid[T] = {
    val ars =in.toVector.map(_.toVector)
    Grid(ars)
  }

  def fromColumns[T](in: List[List[T]]) : Grid[T] = {
    val ars =in.transpose.toVector.map(_.toVector)
    Grid(ars)
  }

}
