setlocal autoindent
setlocal expandtab
setlocal shiftwidth=4
setlocal softtabstop=4
setlocal tabstop=4
setlocal textwidth=88

" This is a pattern for matching macro definitions, but it is sometimes 
" useful to make it match class declarations.
setlocal define=class\\s

let &l:formatprg='pyformatter'


" For running the current test file.
nnoremap <buffer> <leader>tt
      \ :<C-U>write \| compiler pytest \| make %<CR>

" For running all tests.
nnoremap <buffer> <leader>ta
      \ :<C-U>write \| compiler pytest \| make<CR>

" For running linters.
nnoremap <buffer> <leader>l
      \ :<C-U>write \| compiler pylinter \| make<CR>
