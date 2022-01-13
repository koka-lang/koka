" Vim syntax file
" Language: Koka
" Maintainer: Greg Shuflin

if exists("b:current_syntax")
    finish
endif

" Keywords
syn keyword KokaKeyword infix infixr infixl module import as pub abstract 
            \ type struct alias effect con forall exists some 
            \ fun fn val var extern if then else elif match return with in handle handler mask 
            \ ctl final raw override named interface break continue unsafe
            \ co rec open extend behind linear value reference inline noinline initially finally

highlight def link KokaKeyword Keyword

" Functions
syn keyword KokaKeyword fun nextgroup=KokaFunName skipwhite skipempty
syn match KokaFunName /\v[a-z][A-Za-z0-9\-_]*/ display contained
highlight def link KokaFunName Function


" Core Types
syn keyword KokaCoreType any bool cfield char ctail double ediv either float32 global hdiv int int32
            \ int64 local-var maybe optional order ref reuse ssize_t string uint8 vector void
highlight def link KokaCoreType Type

" Comments
syn region KokaLineComment start="//" end="$"
syn region KokaBlockComment matchgroup=KokaBlockComment start="/\*\%(!\|\*[*/]\@!\)\@!" end="\*/" contains=KokaBlockCommentNested
syn region KokaBlockCommentNested matchgroup=KokaBlockComment start="/\*" end="\*/" contains=KokaBlockCommentNested

highlight def link KokaLineComment Comment
highlight def link KokaBlockComment Comment
highlight def link KokaBlockCommentNested Comment

