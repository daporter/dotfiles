setlocal shiftwidth=2
let &softtabstop = &shiftwidth
setlocal expandtab

if executable('css-beautify')
    let &l:formatprg = 'css-beautify -f - -s ' . &shiftwidth
endif

