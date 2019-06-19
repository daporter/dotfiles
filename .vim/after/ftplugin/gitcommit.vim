setlocal spell

if executable('gitlint')
    let &l:makeprg = 'gitlint --msg-filename %'
    let &l:errorformat = '%l: %t%n %m'
endif

if executable('par')
    let &l:formatprg = 'par -w72'
endif

augroup filetype_gitcommit
    autocmd!
    autocmd BufWritePost <buffer> silent make | silent redraw!
augroup END
