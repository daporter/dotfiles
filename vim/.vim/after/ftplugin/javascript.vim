setlocal shiftwidth=2
let &softtabstop = &shiftwidth
setlocal expandtab

if executable('js-beautify')
    let &l:formatprg = 'js-beautify -f - -j -t -s' . &shiftwidth 
endif

if executable('eslint')
    let &l:makeprg='eslint --format compact'
    let &l:errorformat='%f: line %l\, col %c\, %m,%-G%.%#'
endif

command! -buffer Lint update | silent make % | redraw!

nnoremap <buffer> <leader>l :Lint<CR>
