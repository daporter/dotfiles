let g:colors_name = 'dporter'

set background=light

if exists('syntax_on')
    syntax reset
endif

" You can see all the groups currently active with this command:
"     :so $VIMRUNTIME/syntax/hitest.vim

" Highlighting groups.
highlight Normal                     ctermfg=0  ctermbg=7  guifg=fg      guibg=bg
highlight CursorLine   cterm=NONE    ctermfg=fg ctermbg=9
highlight Cursor                     ctermfg=7 ctermbg=0
highlight StatusLine   cterm=reverse ctermfg=fg ctermbg=bg guifg=fg      guibg=bg
highlight StatusLineNC cterm=reverse ctermfg=fg ctermbg=8  guifg=fg      guibg=bg
highlight Visual                     ctermfg=fg ctermbg=9  guifg=fg   guibg=#e5d9bf
highlight SpellBad  cterm=undercurl,italic      ctermfg=1 ctermbg=bg guifg=Red guibg=bg
" highlight LineNr       cterm=italic    ctermfg=4 ctermbg=3 guifg=#918877    guibg=#e2dcd0
" highlight CursorLineNr cterm=italic    guifg=#b5b1a6    guibg=#cbc7bb
" highlight ErrorMsg     ctermfg=1 ctermbg=bg guifg=#c91b00    guibg=bg
highlight Directory cterm=bold                  guifg=fg guibg=bg
" highlight link NonText  Comment
" highlight link Title    Directory
highlight MoreMsg ctermfg=0 ctermbg=7 guifg=fg guibg=bg
" highlight link Question Comment

" Syntax highlighting groups.
highlight Comment    cterm=italic                ctermfg=8
highlight Constant                               ctermfg=fg ctermbg=bg guifg=fg guibg=bg
highlight Identifier cterm=bold                  ctermfg=fg ctermbg=bg guifg=fg guibg=bg
highlight Function   cterm=bold,underline        ctermfg=fg ctermbg=bg guifg=fg guibg=bg
highlight Statement  cterm=italic                ctermfg=fg ctermbg=bg guifg=fg guibg=bg
highlight PreProc    cterm=bold                  ctermfg=fg ctermbg=bg guifg=fg guibg=bg
highlight Type       cterm=bold                  ctermfg=fg ctermbg=bg guifg=fg guibg=bg
highlight Special    cterm=bold                  ctermfg=fg
highlight Underlined                             ctermfg=fg
highlight Ignore                                 ctermfg=8
highlight Error                                  ctermfg=1
highlight Todo       cterm=bold,italic,underline ctermfg=fg ctermbg=bg guifg=fg guibg=bg

" highlight diffLine          cterm=bold      guifg=fg guibg=bg
" highlight diffAdded                         guifg=#73ba9b    guibg=bg
" highlight diffRemoved                       guifg=#ba2d0b    guibg=bg
" highlight gitcommitOverflow                 guifg=#c91b00    guibg=bg

" highlight link helpHyperTextEntry helpHeadline
" highlight link helpSectionDelim   helpHeadline
" highlight link helpHeader         helpHeadline
