" Enable use of shellcheck when editing shell scripts.
if executable('shellcheck')
    setlocal makeprg=shellcheck\ -f\ gcc\ %
endif
