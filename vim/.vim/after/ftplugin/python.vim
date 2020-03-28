setlocal autoindent
setlocal shiftwidth=4
let &softtabstop = &shiftwidth
setlocal expandtab
setlocal textwidth=88

" Turn off hard line wrapping.
setlocal formatoptions-=t

" vim-apathy sets `path', but I'm overriding it to remove all the Python system
" paths that vim-apathy includes.
setlocal path=.,,

" This is a pattern for matching macro definitions, but it is sometimes 
" useful to make it match class declarations.
let &l:define='^\s*class\s\+'

if executable('pyformatter')
    let &l:formatprg='pyformatter'
endif

command! -buffer Check update | compiler py_compile | silent make % | redraw!
command! -buffer Lint update | compiler pylinter | silent make % | redraw!
command! -buffer Pyre update | compiler pyre | silent make | redraw!

nnoremap <buffer> <leader>c :Check<CR>
nnoremap <buffer> <leader>l :Lint<CR>
nnoremap <buffer> <leader>p :Pyre<CR>

nnoremap <buffer> <leader>tl :update \| TestLast<CR>
nnoremap <buffer> <leader>tn :update \| TestNearest<CR>
nnoremap <buffer> <leader>tf :update \| TestFile<CR>
nnoremap <buffer> <leader>ts :update \| TestSuite<CR>
