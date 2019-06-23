" dporter.vim -- Vim color scheme.
" Author:      David Porter (david.a.porter@gmail.com)
" Description: My plan9-based colorscheme.

hi clear

if exists('syntax_on')
  syntax reset
endif

let colors_name = 'dporter'

set background=light

hi  Normal              ctermbg=7   ctermfg=0     cterm=NONE
hi  NonText             ctermbg=7   ctermfg=0     cterm=NONE
hi  Comment             ctermbg=7   ctermfg=12    cterm=italic
hi  Constant            ctermbg=7   ctermfg=0     cterm=NONE
hi  Error               ctermbg=7   ctermfg=1     cterm=bold
hi  Identifier          ctermbg=7   ctermfg=0     cterm=bold
hi  Ignore              ctermbg=7   ctermfg=0     cterm=NONE
hi  PreProc             ctermbg=7   ctermfg=0     cterm=italic
hi  Special             ctermbg=7   ctermfg=0     cterm=NONE
hi  Statement           ctermbg=7   ctermfg=0     cterm=italic
hi  String              ctermbg=7   ctermfg=11    cterm=NONE
hi  Todo                ctermbg=7   ctermfg=0     cterm=bold,italic
hi  Type                ctermbg=7   ctermfg=0     cterm=bold
hi  Underlined          ctermbg=7   ctermfg=0     cterm=underline
hi  StatusLine          ctermbg=11  ctermfg=7     cterm=bold
hi  StatusLineNC        ctermbg=14  ctermfg=11    cterm=NONE
hi  VertSplit           ctermbg=7   ctermfg=0     cterm=NONE
hi  TabLine             ctermbg=7   ctermfg=8     cterm=NONE
hi  TabLineFill         ctermbg=7   ctermfg=8     cterm=NONE
hi  TabLineSel          ctermbg=7   ctermfg=0     cterm=bold
hi  Title               ctermbg=7   ctermfg=0     cterm=bold
hi  CursorLine          ctermbg=7   ctermfg=12    cterm=NONE
hi  LineNr              ctermbg=7   ctermfg=12    cterm=NONE
hi  CursorLineNr        ctermbg=7   ctermfg=1     cterm=NONE
hi  helpLeadBlank       ctermbg=7   ctermfg=0     cterm=NONE
hi  helpHyperTextEntry  ctermbg=7   ctermfg=4     cterm=bold
hi  helpHyperTextJump   ctermbg=7   ctermfg=4     cterm=NONE
hi  helpNormal          ctermbg=7   ctermfg=0     cterm=NONE
hi  Visual              ctermbg=8   ctermfg=fg    cterm=NONE
hi  VisualNOS           ctermbg=6   ctermfg=fg    cterm=NONE
hi  Pmenu               ctermbg=14  ctermfg=0     cterm=NONE
hi  PmenuSbar           ctermbg=14  ctermfg=7     cterm=NONE
hi  PmenuSel            ctermbg=4   ctermfg=7     cterm=NONE
hi  PmenuThumb          ctermbg=12  ctermfg=0     cterm=NONE
hi  Folded              ctermbg=7   ctermfg=5     cterm=italic
hi  WildMenu            ctermbg=15  ctermfg=0     cterm=NONE
hi  SpecialKey          ctermbg=7   ctermfg=0     cterm=bold
hi  DiffAdd             ctermbg=2   ctermfg=7     cterm=NONE
hi  DiffChange          ctermbg=3   ctermfg=7     cterm=NONE
hi  DiffDelete          ctermbg=1   ctermfg=7     cterm=NONE
hi  DiffText            ctermbg=11  ctermfg=0     cterm=underline
hi  IncSearch           ctermbg=2   ctermfg=7     cterm=NONE
hi  Search              ctermbg=2   ctermfg=7     cterm=bold
hi  Directory           ctermbg=7   ctermfg=4     cterm=NONE
hi  MatchParen          ctermbg=10  ctermfg=0     cterm=NONE
hi  SpellBad            ctermbg=7   ctermfg=1     cterm=underline
hi  SpellCap            ctermbg=7   ctermfg=1     cterm=underline
hi  SpellLocal          ctermbg=7   ctermfg=5     cterm=underline
hi  SpellRare           ctermbg=7   ctermfg=1     cterm=underline
hi  ModeMsg             ctermbg=7   ctermfg=0     cterm=bold
hi  MoreMsg             ctermbg=7   ctermfg=0     cterm=bold
hi  Cursor              ctermbg=0   ctermfg=7     cterm=NONE
hi  QuickFixLine        ctermbg=7   ctermfg=NONE  cterm=bold
hi  StatusLineTerm      ctermbg=7   ctermfg=0     cterm=NONE
hi  StatusLineTermNC    ctermbg=7   ctermfg=0     cterm=NONE

hi  link  Number        Constant
hi  link  FoldColumn    LineNr
hi  link  ColorColumn   CursorLine
hi  link  SignColumn    Normal
hi  link  ErrorMsg      Error
hi  link  Question      Normal
hi  link  WarningMsg    Error
hi  link  CursorColumn  CursorLine
