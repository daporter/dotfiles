setlocal tabstop=4
setlocal softtabstop=4
setlocal shiftwidth=4
setlocal textwidth=88
setlocal expandtab
setlocal autoindent

if executable('flake8')
    setlocal makeprg=eval\ 'flake8\ %\ 2>\ /dev/null'
    setlocal errorformat=%f:%l:%c:\ %t%n\ %m
endif
