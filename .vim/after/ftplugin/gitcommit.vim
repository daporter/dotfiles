setlocal spell

if executable('par')
    let &l:formatprg = 'par -w72'
endif

if executable('gitlint')
    let &l:makeprg = 'gitlint --msg-filename'
    let &l:errorformat = '%l: %t%n %m'
endif

command! -buffer Lint update | silent make % | redraw!

nnoremap <buffer> <leader>l :Lint<CR>
