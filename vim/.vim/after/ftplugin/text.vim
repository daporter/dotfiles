setlocal spell

command! -buffer Lint update | compiler textlinter | silent make % | redraw!

nnoremap <buffer> <leader>l :Lint<CR>

