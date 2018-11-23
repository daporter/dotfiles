let g:neomake_text_enabled_makers = ['markdownlint', 'mdl', 'vale', 'proselint', 'writegood']

let &l:formatprg='prettier --stdin --parser=markdown'

augroup filetype_markdown
    autocmd!
    autocmd BufWritePre <buffer> Format     " custom command defined in vimrc
    autocmd BufWritePost <buffer> Neomake
augroup END
