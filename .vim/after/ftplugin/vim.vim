" Enable use of vim-vint when editing vim files.
if executable('vint')
    let &l:makeprg='vint %'
    augroup MyVim
        autocmd!
        autocmd BufWritePost <buffer> silent make % | silent redraw!
    augroup END
endif
