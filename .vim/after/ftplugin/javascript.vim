setlocal tabstop=2
setlocal softtabstop=2
setlocal shiftwidth=2
setlocal expandtab

" Enable use of eslint when editing javascript files.
let &l:makeprg='eslint --format compact %'
let &l:errorformat='%f: line %l\, col %c\, %m,%-G%.%#'

let &l:formatprg='prettier --stdin --parser=babel'
