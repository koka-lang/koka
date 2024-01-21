if exists("b:did_indent")
  finish
endif
let b:did_indent = 1

setlocal indentexpr=GetKokaIndent()

setlocal autoindent

let b:undo_indent = "setlocal autoindent< indentexpr<"

if exists("*GetKokaIndent")
  finish
endif

function! GetKokaIndent()
  " Find a non-blank line above the current line.
  let prevlnum = prevnonblank(v:lnum - 1)

  " Hit the start of the file, use zero indent.
  if prevlnum == 0
    return 0
  endif

  " Add a 'shiftwidth' after lines that usually start a block.
  " Here 'fun', 'ctl' and 'fn()' indent iff they are at the end of a line,
  " or followed by a result type annotation. Checking expr after the type is
  " too hard for regexp.
  let ind = indent(prevlnum)
  let prevline = getline(prevlnum)
  let pos = match(prevline, '
    \\<\%(struct\|type\|effect\|match\|handler\)\>[^}]*$\|
    \\<\%(then\|else\)\s*\%(//.*\)\?$\|
    \\<\%(fun\|ctl\)\>[^(]*([^)]*)\s*\%(:.*\|//.*\)\?$\|
    \\<fn\s*\%(<[^>]*>\s*\)\?([^)]*)\s*\%(:.*\|//.*\)\?$\|
    \[[{(]\s*\%(//.*\)\?$')

  let synattr = synIDattr(synID(prevlnum, pos + 1, 1), "name")
  if pos != -1 && synattr != "KokaBlockComment" && synattr != "KokaLineComment"
    " Exclude inlined 'effect _ val/fun/ctl'.
    if prevline[pos:pos+5] != "effect" || match(prevline, '\<\%(val\|fun\|ctl\)\>', pos) == -1
      let ind = ind + shiftwidth()
    end
  endif

  " Subtract a 'shiftwidth' on block end at line start: 'else', ')', ']', '}'.
  let pos = match(getline(v:lnum), '^\s*\%(else\>\|[]})]\)')
  let synattr = synIDattr(synID(v:lnum, pos + 1, 1), "name") != "KokaBlockComment"
  if pos != -1 && synattr != "KokaBlockComment"
    let ind = ind - shiftwidth()
  endif

  return ind
endfunction
