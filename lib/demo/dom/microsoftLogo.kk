/*----------------------------------------------------------------------------
   Copyright (C) Microsoft Corporation
  
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

module microsoftLogo

import sys/dom
import sys/dom/types
import sys/dom/document
import sys/dom/html/window
import sys/dom/html/htmlElement
import sys/dom/html/htmlTableElement
import sys/dom/html/htmlTableRowElement
import sys/dom/html/htmlTableDataCellElement
import sys/dom/css/cssStyleDeclaration

public fun main() {
  includeLogo()
}

public fun includeLogo(): dom ()
{ 
  withWindow fun(window)
  { 
    val logo = window.document.createMicrosoftLogo(0.5)
    window.document.body.appendChild(logo.node)
    logo.style.position := "fixed"
    logo.style.left     := "15px"
    logo.style.bottom   := "15px"
    logo.style.zIndex  := "1000"
  }
}

public fun createMicrosoftLogo(document: document, scaling: double = 1.0) : dom htmlElement
{ 
  fun pixel(i: int) : string
  { 
    (i.double * scaling).show + "px"
  } 

  val table  = document.createHtmlTableElement()
  val row1   = document.createHtmlTableRowElement()
  val row2   = document.createHtmlTableRowElement()
  val cell1  = document.createHtmlTableDataCellElement()
  val cell2  = document.createHtmlTableDataCellElement()
  val cell3  = document.createHtmlTableDataCellElement()
  val cell4  = document.createHtmlTableDataCellElement()
  val cell5  = document.createHtmlTableDataCellElement()

  table.appendChild(row1.node)
  table.appendChild(row2.node)
  row1.appendChild(cell1.node)
  row1.appendChild(cell2.node)
  row2.appendChild(cell3.node)
  row2.appendChild(cell4.node)

  [cell1,cell2,cell3,cell4].map fun(x) {
    x.style.height               := 50.pixel
    x.style.width                := 50.pixel
  }

  cell1.style.backgroundColor   := "#ee4f12"
  cell2.style.backgroundColor   := "#7fbb00"
  cell3.style.backgroundColor   := "#46a4f2"
  cell4.style.backgroundColor   := "#f5b900"

  cell5.rowSpan                 := 2
  row1.appendChild(cell5.node)
  cell5.style.lineHeight        := 80.pixel
  cell5.style.paddingLeft       := 18.pixel
  cell5.style.fontSize          := 100.pixel
  cell5.style.fontFamily        := "\"Segoe UI\", Frutiger, \"Frutiger Linotype\", \"Myriad Pro\", Myriad, Univers, Calibri, \"Gill Sans\", \"Gill Sans MT\", \"DejaVu Sans Condensed\", \"Liberation Sans\", \"Nimbus Sans L\", Tahoma, Geneva, \"Helvetica Neue\", Helvetica, Arial, sans serif"
  cell5.style.fontWeight        := "bold"

  cell5.style.color              := "white"
  cell5.innerHtml               := "Microsoft"
  
  table.style.borderSpacing     := 5.pixel

  table.htmlElement
}

