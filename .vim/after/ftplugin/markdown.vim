if executable('mdl')
    setlocal errorformat=%f:%l:\ MD%n\ %m
    setlocal makeprg=mdl\ %:S
endif
