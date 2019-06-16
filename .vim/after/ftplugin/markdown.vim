" TODO: set makeprg to: ['markdownlint', 'mdl', 'vale', 'proselint', 'writegood']

setlocal textwidth=80

let &l:formatprg='prettier --stdin --parser=markdown --prose-wrap=always'

compiler markdownlint
