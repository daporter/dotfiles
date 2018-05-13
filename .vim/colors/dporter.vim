let g:colors_name = 'dporter'

set background=light

if exists('syntax_on')
    syntax reset
endif

highlight clear

" You can see all the groups currently active with this command:
"     :so $VIMRUNTIME/syntax/hitest.vim

" Highlighting groups.
highlight Normal       cterm=none             ctermfg=0    ctermbg=7
highlight CursorLine   cterm=none             ctermfg=none ctermbg=8
highlight Cursor       cterm=none             ctermfg=7    ctermbg=0
highlight StatusLine   cterm=reverse          ctermfg=none ctermbg=none
highlight StatusLineNC cterm=reverse          ctermfg=none ctermbg=8
highlight Visual       cterm=none             ctermfg=none ctermbg=8
highlight SpellBad     cterm=undercurl,italic ctermfg=1    ctermbg=none

" highlight LineNr       cterm=italic    ctermfg=4 ctermbg=3 guifg=#918877    guibg=#e2dcd0
" highlight CursorLineNr cterm=italic    guifg=#b5b1a6    guibg=#cbc7bb
" highlight ErrorMsg     ctermfg=1 ctermbg=none guifg=#c91b00    guibg=none
highlight Directory cterm=bold                  
" highlight link NonText  Comment
" highlight link Title    Directory
highlight MoreMsg ctermfg=0 ctermbg=7 
" highlight link Question Comment

" Syntax highlighting groups.
highlight Comment    cterm=italic                ctermfg=9    ctermbg=none
highlight Constant   cterm=none                  ctermfg=none ctermbg=none 
highlight Identifier cterm=bold                  ctermfg=none ctermbg=none
highlight Function   cterm=bold,underline        ctermfg=none ctermbg=none 
highlight Statement  cterm=italic                ctermfg=none ctermbg=none 
highlight PreProc    cterm=bold                  ctermfg=none ctermbg=none 
highlight Type       cterm=bold                  ctermfg=none ctermbg=none 
highlight Special    cterm=bold                  ctermfg=none ctermbg=none 
highlight Underlined cterm=none                  ctermfg=none ctermbg=none
highlight Ignore     cterm=none                  ctermfg=8    ctermbg=none
highlight Error      cterm=none                  ctermfg=1    ctermbg=none
highlight Todo       cterm=bold,italic,underline ctermfg=none ctermbg=none

" highlight diffLine          cterm=bold      guifg=none guibg=none
" highlight diffAdded                         guifg=#73ba9b    guibg=none
" highlight diffRemoved                       guifg=#ba2d0b    guibg=none
" highlight gitcommitOverflow                 guifg=#c91b00    guibg=none

" highlight link helpHyperTextEntry helpHeadline
" highlight link helpSectionDelim   helpHeadline
" highlight link helpHeader         helpHeadline
