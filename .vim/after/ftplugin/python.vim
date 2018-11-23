setlocal tabstop=4
setlocal softtabstop=4
setlocal shiftwidth=4
setlocal textwidth=88
setlocal expandtab
setlocal autoindent

" Set the path to something more suitable for Python projects. This enables
" :find and related commands to do the right thing.
setlocal path=.,**

let &l:formatprg='pyformatter'

augroup filetype_python
    autocmd!
    autocmd BufWritePre <buffer> Format     " custom command defined in vimrc
augroup END

" Neomake configuration.
let g:neomake_python_enabled_makers = ['flake8', 'pydocstyle']

let g:neomake_python_pytest_maker = {
        \ 'exe': 'pytest',
        \ 'args': ['--quiet', '--tb=line'],
        \ 'errorformat': '%-G%.%#%.%#%%%.%#,%-G%.%#FAILURES%.%#,%f:%l:\ %m,%-G%.%#in\ %.%#\ seconds',
        \ }

nnoremap <buffer> <Leader>tt :Neomake pytest<cr>
nnoremap <buffer> <Leader>tp :NeomakeProject pytest<cr>
