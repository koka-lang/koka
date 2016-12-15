/*----------------------------------------------------------------------------
   Copyright (C) Microsoft Corporation
  
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

module starfield

import sys/dom
import sys/dom/types
import sys/dom/document
import sys/dom/html/window
import sys/dom/html/htmlElement
import sys/dom/html/htmlCanvasElement
import sys/dom/html/canvasRenderingContext2d
import sys/dom/css/cssStyleDeclaration

import demo/dom/microsoftLogo

struct star(x: double, y: double, size: double, color: string)
type   direction
       { Forward
         Backward
       }

public fun main() : _ ()
{ 
  val count   = 200
  val timeout = 15
  var starColor       := "white"
  var flightDirection := Forward

  fun moveStars(ctx: canvasRenderingContext2d, stars: list<star>) : dom list<star>
  { 
    ctx.fillStyle := "black"
    val w = ctx.canvas.width.double
    val h = ctx.canvas.height.double
    ctx.fillRect(0.0, 0.0, w, h)
    stars.map fun(star) {
      val newStar = match(flightDirection)
                    { Forward  -> if(star.x.abs > (w/2.0) || star.y.abs > (h/2.0)) {
                                    Star(random() * w - w / 2.0, random() * h - h / 2.0, random() * 4.0, starColor)
                                  } else {
                                    Star(star.x * 1.015, star.y * 1.015, star.size, star.color)
                                  }
                      Backward -> if(random() < 0.01) {
                                    if(random-int().even?) {
                                      Star(random() * w - w / 2.0, if(random-int().even?) { h / 2.0 } else { (0.0-h) / 2.0 } , random() * 4.0, starColor)
                                    } else {
                                      Star(if(random-int().even?) { w / 2.0 } else { (0.0-w) / 2.0 }, random() * h - h / 2.0, random() * 4.0, starColor)
                                    }
                                  } else {
                                    Star(star.x * 0.985, star.y * 0.985, star.size, star.color)
                                  }
                    }
      ctx.fillStyle := star.color
      ctx.fillRect(newStar.x + w / 2.0, newStar.y + h / 2.0, newStar.size, newStar.size) 
      return newStar
    }
  }

  includeLogo()
  withWindow fun(window) {

      val canvas = window.document.createHtmlCanvasElement()
      window.document.body.style.margin := "0px"
      window.document.body.style.overflow := "hidden"
      window.document.body.appendChild(canvas.node)

      val ctx    = canvas.getContext("2d").toCanvasRenderingContext2d
      fun loop(stars: list<star>) : _ ()
      { val newStars = moveStars(ctx, stars)
        window.setTimeout( fun(){ loop(newStars) }, timeout )
        return ()
      }
      canvas.height := window.innerHeight
      canvas.width  := window.innerWidth

      // Handling various events
      window.onresize := (fun(event) {
       canvas.height := window.innerHeight
       canvas.width  := window.innerWidth 
      })
      window.onclick := (fun(event) {
        starColor := "rgb(" + (random-int() % 255).show + "," + (random-int() % 255).show + "," + (random-int() % 255).show + ")"
      })
      window.ondblclick := (fun(event) {
        match(flightDirection)
        { Forward  -> flightDirection := Backward
          Backward -> flightDirection := Forward
        }
      })

      // Starting the loop
      loop( replicate(Star(1000000.0, 1000000.0, 0.0, "white"), count) )
    }
}
