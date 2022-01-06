import std/os/file
import std/os/path
import std/core

struct grid
  rows: list<list<int>>

effect bingo {
  ctl success(value: int): ()
}

effect state {
  ctl get(): list<(int, int)>
  ctl put(row: int, col: int): ()
}

fun getGrid(block:list<string>): grid {
  Grid(block.map(fn(line) { line.split(" ").filter(fn(s) { s != "" }).map(fn (s) { s.parse-int-default(0) }) }))
}

fun getGrids(content: list<string>): (list<grid>, list<int>) {
  val numbersStr = content[0].default("")
  val numbers = numbersStr.split(",").map(fn(s) { parse-int-default(s, 0) })
  var mod_content := content.drop(1)
  val no_grids = mod_content.length() / 6
  val grids = foldl(list(0, no_grids - 1, fn(i){ i }), [], fn(acc, curr) {
    val curr_grid = mod_content.drop(1).take(5).getGrid
    mod_content := mod_content.drop(6)
    acc ++ [curr_grid]
  })
  (grids, numbers)
}

fun sumGrid(grid: grid, marked_positions: list<(int, int)>, num: int): int {
  val sum = map(grid.rows, fn(row) { foldl(row, 0, (+))}).foldl(0, (+))
  val new_sum = foldl(marked_positions, sum, fn(curr_sum, (row, col)) { 
    curr_sum - grid.rows[row].default([])[col].default(0)
  })
  new_sum * num
}

fun checkIfGridWon(positions: list<(int, int)>): bool {
  val rowWon = map(list(0, 4, id), fn(row) { positions.filter(fn((i, _)) { i == row }).length == 5 }).foldl(False, (||))
  val colWon = map(list(0, 4, id), fn(col) { positions.filter(fn((_, j)) { j == col }).length == 5 }).foldl(False, (||))
  rowWon || colWon
}

fun updateMarkedCellsInGrid(grid: grid, num: int): <state, bingo> list<(int, int)> {
  foreach-indexed(grid.rows, fn(i, line) { 
    foreach-indexed(line, fn(j, value) { 
      if num == value then put(i, j)
    })
  })
  val marked_positions = get()
  if checkIfGridWon(marked_positions) then success(grid.sumGrid(marked_positions, num))
  marked_positions
}

fun markNumberInGrid(grid, init_positions: list<(int, int)>, num: int) {
  var st := init_positions
  with handler{
    ctl get() {
      resume(st)
    }
    ctl put(row: int, col: int) {
      st := st.append([(row, col)])
      resume(())
    }
  }
  updateMarkedCellsInGrid(grid, num)
}

fun drawNumbers(grids: list<grid>, numbers: list<int>): bingo () {
  var marked_positions := list(0, grids.length - 1, fn(_) { [] })
  foreach(numbers, fn(num) {
    marked_positions := grids.zip(marked_positions).map(fn((grid, positions)) { 
      markNumberInGrid(grid, positions, num)
    })
  })
}

fun getWinner(grids, numbers): int {
  with handler {
    ctl success(value) -> value
    return(_) -> 0
  }
  drawNumbers(grids, numbers)
}

fun print_list(grid: list<list<int>>) {
  map(grid, fn(line) { 
    map(line, fn(value) { print(value.show ++ " ") })
    println("")
  })
}

fun main() {
  val content = read-text-file(path("day4.txt"))
  val (grids, numbers) = getGrids(content.lines)
  getWinner(grids, numbers)
}