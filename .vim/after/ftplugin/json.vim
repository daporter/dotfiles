" Enable use of eslint when editing javascript files.
if executable('jsonlint')
    setlocal errorformat=%f:\ line\ %l\\,\ col\ %c\\,\ %m,%-G%.%#
    setlocal makeprg=jsonlint\ %\ --compact\ --quiet
endif

augroup my_json
    autocmd!
    autocmd BufWritePre *.json :silent %!prettier --parser json
augroup END
