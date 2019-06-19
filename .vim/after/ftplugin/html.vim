setlocal tabstop=2
setlocal softtabstop=2
setlocal shiftwidth=2
setlocal expandtab

if executable('html-beautify')
    let &l:formatprg = 'html-beautify -f - -I -s ' . &shiftwidth
endif
