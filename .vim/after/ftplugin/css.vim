setlocal tabstop=2
setlocal softtabstop=2
setlocal shiftwidth=2
setlocal expandtab

if executable('css-beautify')
    let &l:formatprg = 'css-beautify -f - -s ' . &shiftwidth
endif

