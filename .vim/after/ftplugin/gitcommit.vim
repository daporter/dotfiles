setlocal spell

if executable('gitlint')
    setlocal makeprg=gitlint\ <\ %:S
    setlocal errorformat=%l:\ %t%n\ %m:\ \"%s\"
endif
