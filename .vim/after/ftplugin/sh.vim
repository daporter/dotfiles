" Enable use of shellcheck when editing shell scripts.
if executable('shellcheck')
    let &l:makeprg = 'shellcheck -f gcc'
endif

command! -buffer Lint update | silent make % | redraw!

nnoremap <buffer> <leader>l :Lint<CR>
