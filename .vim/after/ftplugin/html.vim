setlocal tabstop=2
setlocal softtabstop=2
setlocal shiftwidth=2
setlocal expandtab

let &l:formatprg='tidy --indent yes --wrap 0 --tidy-mark no --force-output true -quiet --show-errors 0 --show-warnings 0'
