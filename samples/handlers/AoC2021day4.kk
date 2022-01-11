import std/os/file
import std/os/path
import std/core

alias grid = list<maybe<int>>

effect bingo
  ctl success(value: int) : ()
  fun grid-won(): ()
  fun game-over(first: bool): bool

fun get-grid(block:list<string>): grid
  block.map(fn(line) line.split(" ").filter(fn(s) s != "" ).map(fn(s) Just(s.parse-int-default(0)) )).concat

fun get-grids(content: list<string>): (list<grid>, list<int>)
  val numbers = content[0].default("").split(",").map(fn(s) parse-int-default(s, 0) )
  var mod-content := content.drop(1)
  val no-grids = mod-content.length() / 6
  val grids = foldl(list(0, no-grids - 1, id), [], fn(acc, curr)
    val curr-grid = mod-content.drop(1).take(5).get-grid
    mod-content := mod-content.drop(6)
    acc ++ [curr-grid]
  )
  (grids, numbers)

fun rows( g : grid ) : div list<list<maybe<int>>>
  if g.is-nil then [] else Cons( g.take(5), g.drop(5).rows )

fun transpose( xss : list<list<a>> ) : list<list<a>>
  list(0, xss.length - 1, id).map( fn(i) xss.map(fn(xs) xs.drop(i).take(1) ).concat)

fun columns( g : grid ) : div list<list<maybe<int>>>
  g.rows.transpose

fun sum-grid(grid: grid, num: int): int
  grid.map( fn(x) x.default(0) ).sum * num

fun or(xss: list<bool>): bool
  xss.foldl(False, (||))

fun check-if-grid-won(grid: grid): div bool
  val row-won = grid.rows.map(fn(row) row.foldl(True, fn(acc, curr) if acc && curr.is-nothing then True else False )).or
  val col-won = grid.columns.map(fn(col) col.foldl(True, fn(acc, curr) if acc && curr.is-nothing then True else False )).or
  row-won || col-won

fun mark-number-in-grid(grid: grid, num: int, first: bool): <bingo,div,console> maybe<grid>
  val mod_grid = grid.map(fn(value)
    match(value)
      Just(jval) -> if jval == num then Nothing else Just(jval)
      Nothing -> Nothing 
  )
  if mod_grid.check-if-grid-won then 
    grid-won()
    if game-over(first) then success(mod_grid.sum-grid(num))
    Nothing
  else Just(mod_grid)

fun draw-numbers( grids : list<grid>, numbers : list<int>, first: bool) : <bingo,div,console> ()
  var players := grids
  numbers.foreach fn(num)
    players := players.filter-map fn(grid) 
      grid.mark-number-in-grid(num, first)

fun get-winner(grids: list<grid>, numbers: list<int>, first: bool) : <div,console> int
  var winners := 0
  with handler 
    ctl success(value) value
    fun grid-won() winners := winners + 1
    fun game-over(first-winner: bool) 
      if first-winner then True
      else winners == grids.length
  draw-numbers(grids, numbers, first)
  0

pub fun main()
  val content = read-text-file(path("C:/Users/rakb/Documents/Personal/Code/experiments_desktop/Koka/AdventOfCode2021/data/day4.txt"))
  val (grids, numbers) = get-grids(content.lines)
  val first_winner_sum = get-winner(grids, numbers, True) // First winner
  val last_winner_sum = get-winner(grids, numbers, False) // Last winner
  println("First winner: " ++ first_winner_sum.show ++ ", last winner: " ++ last_winner_sum.show)