setlocal tabstop=4
setlocal softtabstop=4
setlocal shiftwidth=4
setlocal textwidth=88
setlocal expandtab
setlocal autoindent

nnoremap <buffer> <Leader>b :Black<cr>
nnoremap <buffer> <Leader>f :compiler flake8<cr>:make<cr>
nnoremap <buffer> <Leader>t :compiler pytest<cr>:make<cr>
