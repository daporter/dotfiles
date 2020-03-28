" TODO: set makeprg to: ['markdownlint', 'mdl', 'vale', 'proselint', 'writegood']

setlocal textwidth=80

if executable('prettier')
    let &l:formatprg='prettier --stdin --parser=markdown --prose-wrap=always'
endif

command! -buffer Lint update | compiler markdownlint | silent make % | redraw!

nnoremap <buffer> <leader>l :Lint<CR>
