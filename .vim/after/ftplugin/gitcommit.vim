setlocal spell

setlocal makeprg=gitlint\ --msg-filename\ %
setlocal errorformat=%l:\ %t%n\ %m
setlocal formatprg=par\ -w72

augroup filetype_gitcommit
    autocmd!
    autocmd BufWritePost <buffer> silent make <bar> silent redraw!
augroup END
