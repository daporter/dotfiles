if executable('proselint')
    setlocal makeprg=proselint\ %:S
endif
