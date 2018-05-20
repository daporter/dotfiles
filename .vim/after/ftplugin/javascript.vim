setlocal ts=2 sts=2 sw=2 expandtab

" Enable use of eslint when editing javascript files.
if executable('eslint')
    setlocal errorformat=%f:\ line\ %l\\,\ col\ %c\\,\ %m,%-G%.%#
    setlocal makeprg=eslint\ --format\ compact\ %
    augroup MyJavascript
        autocmd!
        autocmd BufWritePost <buffer> silent make % | silent redraw!
    augroup END
endif

" Use 'prettier' to format javascript code.
if executable('prettier')
    let &l:formatprg='prettier --stdin --print-width 80 --single-quote --trailing-comma es5'
endif
