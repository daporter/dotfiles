setlocal shiftwidth=2
let &softtabstop = &shiftwidth
setlocal expandtab
setlocal smartindent
setlocal autoindent

if executable('hindent')
    let &l:formatprg='hindent'
endif
