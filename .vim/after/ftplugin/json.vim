" Enable use of eslint when editing javascript files.
if executable('jsonlint')
    let &l:errorformat = '%f: line %l, col %c, %m,%-G%.%#'
    let &l:makeprg = 'jsonlint % --compact --quiet'
endif

if executable('js-beautify')
    let &l:formatprg = 'js-beautify -f - -j -t -s' . &shiftwidth 
endif
