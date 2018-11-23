let g:neomake_text_enabled_makers = ['proselint', 'writegood']

augroup filetype_text
    autocmd!
    autocmd BufWritePost <buffer> Neomake
augroup END
