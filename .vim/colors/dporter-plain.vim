" dporter.vim -- Vim color scheme.
" Author:      David Porter (david.a.porter@gmail.com)
" Description: My plan9-based colorscheme.

hi clear

if exists('syntax_on')
  syntax reset
endif

let colors_name = 'dporter-plain'

set background=dark

hi  Comment             ctermbg=NONE  ctermfg=8     cterm=italic
hi  Constant            ctermbg=NONE  ctermfg=6     cterm=NONE
hi  Cursor              ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi  CursorLine          ctermbg=10    ctermfg=NONE  cterm=NONE
hi  CursorLineNr        ctermbg=NONE  ctermfg=1     cterm=NONE
hi  DiffAdd             ctermbg=2     ctermfg=0     cterm=NONE
hi  DiffChange          ctermbg=11    ctermfg=NONE  cterm=NONE
hi  DiffDelete          ctermbg=1     ctermfg=NONE  cterm=NONE
hi  DiffText            ctermbg=3     ctermfg=0     cterm=NONE
hi  Directory           ctermbg=NONE  ctermfg=4     cterm=NONE
hi  Error               ctermbg=NONE  ctermfg=1     cterm=bold
hi  FoldColumn          ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi  Folded              ctermbg=NONE  ctermfg=5     cterm=italic
hi  Identifier          ctermbg=NONE  ctermfg=NONE  cterm=bold
hi  Ignore              ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi  IncSearch           ctermbg=4     ctermfg=10    cterm=bold
hi  LineNr              ctermbg=NONE  ctermfg=14    cterm=NONE
hi  MatchParen          ctermbg=6     ctermfg=15    cterm=NONE
hi  ModeMsg             ctermbg=NONE  ctermfg=NONE  cterm=bold
hi  MoreMsg             ctermbg=NONE  ctermfg=NONE  cterm=bold
hi  NonText             ctermbg=NONE  ctermfg=11    cterm=NONE
hi  Normal              ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi  Pmenu               ctermbg=12    ctermfg=0     cterm=NONE
hi  PmenuSbar           ctermbg=12    ctermfg=NONE  cterm=NONE
hi  PmenuSel            ctermbg=4     ctermfg=15    cterm=NONE
hi  PmenuThumb          ctermbg=4     ctermfg=NONE  cterm=NONE
hi  PreProc             ctermbg=NONE  ctermfg=NONE  cterm=italic
hi  QuickFixLine        ctermbg=NONE  ctermfg=NONE  cterm=bold
hi  Search              ctermbg=2     ctermfg=7     cterm=bold
hi  Special             ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi  SpecialKey          ctermbg=NONE  ctermfg=NONE  cterm=bold
hi  SpellBad            ctermbg=NONE  ctermfg=1     cterm=underline
hi  SpellCap            ctermbg=NONE  ctermfg=1     cterm=underline
hi  SpellLocal          ctermbg=NONE  ctermfg=5     cterm=underline
hi  SpellRare           ctermbg=NONE  ctermfg=1     cterm=underline
hi  Statement           ctermbg=NONE  ctermfg=NONE  cterm=italic
hi  StatusLine          ctermbg=NONE  ctermfg=NONE  cterm=reverse,bold,italic
hi  StatusLineNC        ctermbg=7     ctermfg=NONE  cterm=italic
hi  StatusLineTerm      ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi  StatusLineTermNC    ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi  StatusLineTermNC    ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi  String              ctermbg=NONE  ctermfg=2     cterm=NONE
hi  TabLine             ctermbg=NONE  ctermfg=8     cterm=NONE
hi  TabLineFill         ctermbg=NONE  ctermfg=8     cterm=NONE
hi  TabLineSel          ctermbg=NONE  ctermfg=NONE  cterm=bold
hi  Title               ctermbg=NONE  ctermfg=NONE  cterm=bold
hi  Todo                ctermbg=NONE  ctermfg=NONE  cterm=bold,italic
hi  Type                ctermbg=NONE  ctermfg=NONE  cterm=bold
hi  Underlined          ctermbg=NONE  ctermfg=NONE  cterm=underline
hi  VertSplit           ctermbg=NONE  ctermfg=0     cterm=NONE
hi  Visual              ctermbg=14    ctermfg=NONE  cterm=NONE
hi  VisualNOS           ctermbg=6     ctermfg=NONE  cterm=NONE
hi  WildMenu            ctermbg=2     ctermfg=15    cterm=bold
hi  helpHyperTextEntry  ctermbg=NONE  ctermfg=4     cterm=bold
hi  helpHyperTextJump   ctermbg=NONE  ctermfg=4     cterm=NONE
hi  helpLeadBlank       ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi  helpNormal          ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi  qfLineNr            ctermbg=NONE  ctermfg=13    cterm=NONE
hi  rustEnumVariant	    ctermbg=NONE  ctermfg=NONE  cterm=NONE

hi  link  ColorColumn     CursorLine
hi  link  CursorColumn    CursorLine
hi  link  ErrorMsg        Error
hi  link  FoldColumn      LineNr
hi  link  Number          Constant
hi  link  Question        Normal
hi  link  SignColumn      Normal
hi  link  SpecialComment  Comment
hi  link  WarningMsg      Error
