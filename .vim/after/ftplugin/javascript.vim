setlocal tabstop=2
setlocal softtabstop=2
setlocal shiftwidth=2
setlocal expandtab

" Enable use of eslint when editing javascript files.
let &l:errorformat='%f: line %l\, col %c\, %m,%-G%.%#'
let &l:makeprg='eslint --format compact %'
let &l:formatprg='prettier --stdin --parser=babel'

" automatically run my Format command whenever the file is written.
augroup filetype_javascript
    autocmd!
    autocmd BufWritePre <buffer> Format     " custom command defined in vimrc
    autocmd BufWritePost <buffer> silent make % | silent redraw!
augroup END
