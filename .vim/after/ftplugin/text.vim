" For running linters.
nnoremap <buffer> <leader>l
      \ :<C-U>write \| compiler textlinter \| make<CR>
