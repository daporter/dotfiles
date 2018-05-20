setlocal spell
setlocal formatprg=par\ -w72

if executable('gitlint')
    setlocal makeprg=gitlint\ <\ %:S
    setlocal errorformat=%l:\ %t%n\ %m:\ \"%s\"
endif
