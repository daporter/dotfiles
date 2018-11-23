" Enable use of eslint when editing javascript files.
if executable('jsonlint')
    setlocal errorformat=%f:\ line\ %l\\,\ col\ %c\\,\ %m,%-G%.%#
    setlocal makeprg=jsonlint\ %\ --compact\ --quiet
endif

let &l:formatprg='prettier --stdin --parser=json'

augroup filetype_json
    autocmd!
    autocmd BufWritePre <buffer> Format     " custom command defined in vimrc
augroup END
