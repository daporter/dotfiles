highlight clear 

highlight! Normal                       guifg=#140c01    guibg=#f8f2e4
highlight! CursorLine   cterm=NONE      guibg=#e2dcd0
highlight! StatusLine                   guifg=#3a362a    guibg=#e2dcd0 
highlight! StatusLineNC                 guifg=#69645d    guibg=#cbc7bb
highlight! Visual                       guibg=#e5d9bf
highlight! LineNr       cterm=italic    guifg=#918877    guibg=#e2dcd0
highlight! CursorLineNr cterm=italic    guifg=#b5b1a6    guibg=#cbc7bb
highlight! Underlined   cterm=underline guifg=foreground guibg=background 

highlight! link VertSplit StatusLineNC

highlight! Statement  cterm=italic         guifg=foreground guibg=background 
highlight! Type       cterm=bold           guifg=foreground guibg=background
highlight! Constant                        guifg=foreground guibg=background
highlight! Identifier cterm=bold           guifg=foreground guibg=background
highlight! Function   cterm=bold,underline guifg=foreground guibg=background
highlight! PreProc                         guifg=foreground guibg=background
highlight! Comment    cterm=italic         guifg=#94908b
highlight! Special                         guifg=foreground guibg=background

highlight! Directory cterm=bold                  guifg=foreground guibg=background
highlight! Todo      cterm=bold,italic,underline guifg=foreground guibg=background
" highlight! SpecialKey cterm=bold   guifg=foreground guibg=background
highlight! ErrorMsg   guifg=#c91b00    guibg=background

" highlight! link NonText  Comment
" highlight! link Title    Directory
" highlight! link MoreMsg  Comment
" highlight! link Question Comment

highlight! diffLine          cterm=bold      guifg=foreground guibg=background
highlight! diffAdded                         guifg=#73ba9b    guibg=background
highlight! diffRemoved                       guifg=#ba2d0b    guibg=background

" Vim.
highlight! vimOption         cterm=bold      guifg=foreground guibg=background
highlight! vimFuncName       cterm=NONE      guifg=foreground guibg=background
highlight! vimFunction       cterm=bold      guifg=foreground guibg=background
highlight! vimIsCommand      cterm=bold      guifg=foreground guibg=background
highlight! helpHyperTextJump cterm=underline guifg=foreground guibg=background
highlight! helpHeadline      cterm=bold      guifg=foreground guibg=background
highlight! helpExample       cterm=italic    guifg=foreground guibg=background

highlight! link helpHyperTextEntry helpHeadline
highlight! link helpSectionDelim   helpHeadline
highlight! link helpHeader         helpHeadline

highlight! rubyDefine           cterm=italic guifg=foreground guibg=background
highlight! rubyClass            cterm=italic guifg=foreground guibg=background
highlight! rubyInstanceVariable              guifg=foreground guibg=background

let g:colors_name = 'dporter'
