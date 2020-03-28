setlocal shiftwidth=2
let &softtabstop = &shiftwidth
setlocal expandtab

if executable('js-beautify')
    let &l:formatprg = 'js-beautify -f - -j -t -s' . &shiftwidth 
endif

if executable('jsonlint')
    let &l:errorformat = '%f: line %l, col %c, %m,%-G%.%#'
    let &l:makeprg = 'jsonlint % --compact --quiet'
endif

command! -buffer Lint update | silent make % | redraw!

nnoremap <buffer> <leader>l :Lint<CR>
