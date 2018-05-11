if executable('proselint')
    setlocal makeprg=proselint\ %:S
    augroup MyMarkdown
        autocmd!
        autocmd BufWritePost <buffer> silent make % | silent redraw!
    augroup END
endif
