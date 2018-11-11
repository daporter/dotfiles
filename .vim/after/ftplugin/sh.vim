" Enable use of shellcheck when editing shell scripts.
if executable('shellcheck')
    setlocal makeprg=shellcheck\ -f\ gcc\ %
endif

" Use 'shfmt' to format shell scripts.
if executable('shfmt')
    setlocal formatprg=shfmt\ -s\ -i\ 4
endif
