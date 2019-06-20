setlocal autoindent
setlocal expandtab
setlocal shiftwidth=4
setlocal softtabstop=4
setlocal tabstop=4
setlocal textwidth=88

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

nnoremap <buffer> <leader>c :Check<CR>
nnoremap <buffer> <leader>l :Lint<CR>
