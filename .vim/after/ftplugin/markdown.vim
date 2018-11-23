if executable('mdl')
    setlocal errorformat=%f:%l:\ MD%n\ %m
    setlocal makeprg=mdl\ %:S
endif

let &l:formatprg='prettier --stdin --parser=markdown'

" automatically run my Format command whenever the file is written.
augroup filetype_markdown
    autocmd!
    autocmd BufWritePre <buffer> Format     " custom command defined in vimrc
    autocmd BufWritePost <buffer> silent make % | silent redraw!
augroup END
