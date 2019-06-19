setlocal tabstop=2
setlocal expandtab
setlocal shiftwidth=2
setlocal smartindent
setlocal autoindent

if executable('hindent')
    let &l:formatprg='hindent'
endif
