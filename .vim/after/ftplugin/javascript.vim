" Enable use of eslint when editing javascript files.
if executable('eslint')
    setlocal errorformat=%f:\ line\ %l\\,\ col\ %c\\,\ %m,%-G%.%#
    setlocal makeprg=eslint\ --format\ compact
    autocmd BufWritePost <buffer> silent make! % | silent redraw!
endif

" Use 'prettier' to format javascript code.
if executable('prettier')
    let &l:formatprg='prettier --stdin --print-width 80 --single-quote --trailing-comma es5'
endif
