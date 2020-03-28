setlocal shiftwidth=2
let &softtabstop = &shiftwidth
setlocal expandtab

if executable('html-beautify')
    let &l:formatprg = 'html-beautify -f - -I -s ' . &shiftwidth
endif
