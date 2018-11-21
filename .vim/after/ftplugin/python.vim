setlocal tabstop=4
setlocal softtabstop=4
setlocal shiftwidth=4
setlocal textwidth=88
setlocal expandtab
setlocal autoindent

" Set the path to something more suitable for Python projects. This enables
" :find and related commands to do the right thing.
setlocal path=.,**

nnoremap <buffer> <Leader>tt :compiler pytest<cr>:make %<cr>
nnoremap <buffer> <Leader>ta :compiler pytest<cr>:make<cr>

" Neoformat configuration.
let g:neoformat_run_all_formatters = 1
let g:neoformat_enabled_python = ['black', 'isort', 'docformatter']

" Neomake configuration.
let g:neomake_python_enabled_makers = ['flake8', 'pydocstyle']
