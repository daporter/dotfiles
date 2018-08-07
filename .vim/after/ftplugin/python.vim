setlocal tabstop=4
setlocal softtabstop=4
setlocal shiftwidth=4
setlocal textwidth=79
setlocal expandtab
setlocal autoindent

if executable('flake8')
    setlocal makeprg=flake8\ --format=default\ %
    setlocal errorformat=%f:%l:%c:\ %t%n\ %m
endif
