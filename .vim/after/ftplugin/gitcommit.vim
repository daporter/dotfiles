setlocal spell

let &l:makeprg='gitlint < %:S'
let &l:errorformat='%l: %t%n %m: "%s"'
let &l:formatprg='par -w72'

augroup filetype_gitcommit
    autocmd!
    autocmd BufWritePost <buffer> silent make % | silent redraw!
augroup END
