" Enable use of vim-vint when editing vim files.
if executable('vint')
    let &l:makeprg='vint'
endif

command! -buffer Lint update | silent make % | redraw!

nnoremap <buffer> <leader>l :Lint<CR>
