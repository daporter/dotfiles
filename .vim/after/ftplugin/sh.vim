" Enable use of shellcheck when editing shell scripts.
if executable('shellcheck')
    setlocal makeprg=shellcheck\ -f\ gcc\ %
    augroup sh
        autocmd!
        autocmd BufWritePost <buffer> silent make % | silent redraw!
    augroup END
endif

" Use 'shfmt' to format shell scripts.
if executable('shfmt')
    setlocal formatprg=shfmt\ -s\ -i\ 4
endif
