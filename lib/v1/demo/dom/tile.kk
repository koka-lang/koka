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
import elem = sys/dom/html/htmlElement
import sys/dom/html/htmlDivElement
import sys/dom/html/htmlSpanElement
import sys/dom/html/htmlInputElement
import sys/dom/html/htmlTextAreaElement
import sys/dom/css/cssStyleDeclaration

abstract struct htmlTileElement( element:     htmlElement,
                                 container:   htmlElement,
                                 captionElem:     htmlElement
                               )

public function caption( tile : htmlTileElement, assigned : string ) {
  tile.captionElem.innerHtml := assigned;
}

public function appendChild( tile : htmlTileElement, elem : node ) {
  tile.container.elem/appendChild(elem);
}

public function createHtmlTileElement(document: document) : <dom,ndet> htmlTileElement
{ 
  val tile      = document.createHtmlDivElement()
  val captionElem = document.createHtmlSpanElement()
  val container    = document.createHtmlDivElement()

  tile.className := "tile"
  captionElem.innerHtml   := "tile"
  captionElem.className   := "caption"
  container.className := "container"
  tile.appendChild(captionElem.node)
  tile.appendChild(container.node)

  document.body.appendChild(tile.node)

  return HtmlTileElement( tile.htmlElement(), container.htmlElement(), captionElem.htmlElement )
}
