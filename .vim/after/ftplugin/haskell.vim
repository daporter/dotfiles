setlocal tabstop=2
setlocal expandtab
setlocal shiftwidth=2
setlocal smartindent
setlocal autoindent

let &l:formatprg='hindent'

augroup filetype_haskell
    autocmd!
    autocmd BufWritePre <buffer> Format     " custom command defined in vimrc
augroup END

