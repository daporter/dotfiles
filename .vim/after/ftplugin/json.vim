" Enable use of eslint when editing javascript files.
if executable('jsonlint')
    setlocal errorformat=%f:\ line\ %l\\,\ col\ %c\\,\ %m,%-G%.%#
    setlocal makeprg=jsonlint\ %\ --compact\ --quiet
endif

" Neoformat configuration.
let g:neoformat_run_all_formatters = 0
let g:neoformat_enabled_json = ['prettier']
