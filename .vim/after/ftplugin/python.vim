setlocal tabstop=4
setlocal softtabstop=4
setlocal shiftwidth=4
setlocal textwidth=88
setlocal expandtab
setlocal autoindent

" Set the path to something more suitable for Python projects. This enables
" :find and related commands to do the right thing.
setlocal path=.,**

setlocal define=class\\s

let &l:formatprg='pyformatter'


" For running tests.
nnoremap <buffer> <leader>t
      \ :<C-U>write \| compiler pytest \| make<CR>

" For running linters.
nnoremap <buffer> <leader>l
      \ :<C-U>write \| compiler pylinter \| make<CR>
