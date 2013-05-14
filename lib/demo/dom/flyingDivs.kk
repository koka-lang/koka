/*----------------------------------------------------------------------------
   Copyright (C) Microsoft Corporation
  
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

import sys/dom
import sys/dom/types
import sys/dom/document
import sys/dom/html/window        
import sys/dom/html/htmlElement
import sys/dom/html/htmlSpanElement 
import sys/dom/css/cssStyleDeclaration

public function main() : dom ()
{ 
  withWindow fun(window : window) 
  {
    val span1 = window.document.createHtmlSpanElement()
    val span2 = window.document.createHtmlSpanElement()
    val span3 = window.document.createHtmlSpanElement()
    
    span1.innerHtml := "This is Koka!"
    span2.innerHtml := "Enjoy our new typesafe language!"
    span3.innerHtml := "Koka is all about having fun!"

    span1.style.position := "absolute"
    span2.style.position := "absolute"
    span3.style.position := "absolute"
    span1.style.padding  := "10px"
    span2.style.padding  := "10px"
    span3.style.padding  := "10px"

    window.document.body.appendChild(span1.node)
    window.document.body.appendChild(span2.node)
    window.document.body.appendChild(span3.node)

    function place(element : htmlSpanElement)
    { 
      val x = randomInt() % 500
      val y = randomInt() % 500
      element.style.left := x.show + "px"
      element.style.top  := y.show + "px"
      val r = (randomInt() % 100) + 155
      val g = (randomInt() % 100) + 155
      val b = (randomInt() % 100) + 155
      element.style.backgroundColor := "rgb(" + r.show + "," + g.show + "," + b.show + ")"
    } 

    function renew()
    { 
      place(span1)
      place(span2)
      place(span3)
    }

    renew()
    interval( 1000, renew)
  }
}
