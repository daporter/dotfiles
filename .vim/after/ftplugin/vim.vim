" Enable use of vim-vint when editing vim files.
if executable('vint')
    setlocal makeprg=vint\ %
    augroup vim
        autocmd!
        autocmd BufWritePost <buffer> silent make! % | silent redraw!
    augroup END
endif
