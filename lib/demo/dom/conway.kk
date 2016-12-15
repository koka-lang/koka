/*----------------------------------------------------------------------------
   Copyright (C) Microsoft Corporation
  
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

// Demo of HTML5 Canvas.

import sys/dom
import sys/dom/types
import sys/dom/document
import sys/dom/html/window
import sys/dom/html/htmlElement
import sys/dom/html/htmlCanvasElement
import sys/dom/html/canvasRenderingContext2d

import demo/dom/tile

struct cell( row : int, col : int, alive : bool )
alias  world = list<list<cell>>
 
public fun main() { 
  withWindow fun(w) { 
    gameOfLife(w) 
  } 
}

fun gameOfLife( window:   window,
                       gridSize: int                  = 50,
                       lineColor: string              = "#cdcdcd",
                       liveColor: string              = "green",
                       deadColor: string              = "#eee",
                       initialLifeProbability: double = 0.1,
                       cellSize: double               = 8.0) : <dom,console,div> () 
{   
  val tile   = window.document.createHtmlTileElement()
  val canvas = window.document.createHtmlCanvasElement()
  tile.caption := "Conway's game of life"
  
  var framecounter := 0
  tile.appendChild(canvas.node)
  canvas.height := 400
  canvas.width := 400

  fun isAlive(world : world, row : int, col : int) : exn bool {  
    ((world[row % gridSize].unjust)[col % gridSize]).unjust.alive
  }

  fun countNeighbors( w : world, c : cell ) : exn int {
    [ w.isAlive(c.row - 1, c.col - 1),
      w.isAlive(c.row - 1, c.col), 
      w.isAlive(c.row - 1, c.col + 1), 
      w.isAlive(c.row, c.col - 1), 
      w.isAlive(c.row, c.col + 1), 
      w.isAlive(c.row + 1, c.col - 1), 
      w.isAlive(c.row + 1, c.col), 
      w.isAlive(c.row + 1, c.col + 1) ].filter(id).length
  }
 
  fun createWorld() : ndet world { 
    list(0, gridSize - 1).map fun(r) {
      list(0, gridSize - 1).map fun(c) { 
        Cell(r,c, random() < initialLifeProbability)
      }
    } 
  }

  fun mapWorld( w : world, f : cell -> e a ) : e list<list<a>> {
    w.map(fun(xs) { xs.map(f) }) 
  } 

  fun evolve( w : world ) : exn world {
    w.mapWorld fun(c) {
      val count = countNeighbors(w,c)
      if(w.isAlive(c.row, c.col)) {
        c( alive = ( count == 2 || count == 3 ) ) 
      } else {
        c( alive = count == 3 ) 
      }
    } 
  }

  fun draw(ctx : canvasRenderingContext2d, c : cell) : dom () { 
    ctx.strokeStyle := lineColor
    ctx.strokeRect(c.row.double * cellSize, c.col.double * cellSize, cellSize, cellSize)
    if(c.alive) {
      ctx.fillStyle := liveColor 
    } else {
      ctx.fillStyle := deadColor
    }
    ctx.fillRect(c.row.double * cellSize, c.col.double * cellSize, cellSize, cellSize)
  }   

  fun loop(ctx : canvasRenderingContext2d, world : world) : <dom,div> () 
  { 
    framecounter := framecounter + 1
    world.mapWorld fun(c) { draw(ctx, c) } 
    timeout(0) fun() { loop(ctx, world.evolve) }
  }


  fun measurePerformance() 
  { 
    println( framecounter.show + "fps" )
    framecounter := 0 
  }

  loop(canvas.getContext("2d").toCanvasRenderingContext2d, createWorld())
  interval(1000, measurePerformance)
}
