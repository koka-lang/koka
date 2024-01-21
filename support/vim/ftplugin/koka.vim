if exists("b:did_ftplugin")
  finish
endif
let b:did_ftplugin = 1

let s:cpo_save = &cpo
set cpo&vim

setlocal comments=s1:/*,mb:*,ex:*/,://
setlocal commentstring=//%s
setlocal formatoptions-=t formatoptions+=croql

setlocal suffixesadd=.kk

setlocal tabstop=2 shiftwidth=2 softtabstop=2 expandtab

" Fallback when 'indentexpr' is not enabled.
setlocal smartindent

let b:undo_ftplugin = "setlocal comments< commentstring< formatoptions< suffixesadd< tabstop< shiftwidth< softtabstop< expandtab< smartindent<"

let &cpo = s:cpo_save
unlet s:cpo_save

" vim: nowrap sw=2 sts=2 ts=8 noet:
