
" dporter-colour.vim -- Vim color scheme.
" Author:      David Porter (david.a.porter@gmail.com)
" Description: My coloured colorscheme.

hi clear

if exists('syntax_on')
  syntax reset
endif

let colors_name = 'dporter-colour'

set background=dark

hi   Normal              ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi   NonText             ctermbg=NONE  ctermfg=8     cterm=NONE
hi   Comment             ctermbg=NONE  ctermfg=11    cterm=italic
hi   Constant            ctermbg=NONE  ctermfg=4     cterm=NONE
hi   Error               ctermbg=NONE  ctermfg=9     cterm=reverse
hi   Identifier          ctermbg=NONE  ctermfg=NONE  cterm=bold
"hi  Ignore              ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi   PreProc             ctermbg=NONE  ctermfg=NONE  cterm=italic
hi   Special             ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi   Statement           ctermbg=NONE  ctermfg=NONE  cterm=italic
hi   String              ctermbg=NONE  ctermfg=2     cterm=NONE
hi   Todo                ctermbg=NONE  ctermfg=NONE  cterm=bold,italic
hi   Type                ctermbg=NONE  ctermfg=NONE  cterm=bold
hi   Underlined          ctermbg=NONE  ctermfg=NONE  cterm=underline
hi   StatusLine          ctermbg=11    ctermfg=0     cterm=None
hi   StatusLineNC        ctermbg=8     ctermfg=11    cterm=NONE
hi   VertSplit           ctermbg=NONE  ctermfg=11    cterm=NONE
hi   TabLine             ctermbg=NONE  ctermfg=8     cterm=NONE
hi   TabLineFill         ctermbg=NONE  ctermfg=8     cterm=NONE
hi   TabLineSel          ctermbg=NONE  ctermfg=NONE  cterm=bold
hi   Title               ctermbg=NONE  ctermfg=NONE  cterm=bold
hi   CursorLine          ctermbg=10    ctermfg=NONE  cterm=NONE
hi   LineNr              ctermbg=NONE  ctermfg=14    cterm=NONE
hi   CursorLineNr        ctermbg=NONE  ctermfg=1     cterm=NONE
"hi  helpLeadBlank       ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi   helpHyperTextEntry  ctermbg=NONE  ctermfg=4     cterm=bold
hi   helpHyperTextJump   ctermbg=NONE  ctermfg=4     cterm=NONE
"hi  helpNormal          ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi   Visual              ctermbg=8     ctermfg=NONE  cterm=NONE
hi   VisualNOS           ctermbg=6     ctermfg=NONE  cterm=NONE
hi   Pmenu               ctermbg=8     ctermfg=13    cterm=NONE
hi   PmenuSbar           ctermbg=8     ctermfg=NONE  cterm=NONE
hi   PmenuSel            ctermbg=11    ctermfg=15    cterm=NONE
hi   PmenuThumb          ctermbg=11    ctermfg=NONE  cterm=NONE
hi   Folded              ctermbg=NONE  ctermfg=5     cterm=italic
"hi  WildMenu            ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi   SpecialKey          ctermbg=NONE  ctermfg=NONE  cterm=bold
hi   DiffAdd             ctermbg=10    ctermfg=NONE  cterm=NONE
hi   DiffDelete          ctermbg=9     ctermfg=NONE  cterm=NONE
hi   DiffChange          ctermbg=8     ctermfg=NONE  cterm=NONE
hi   DiffText            ctermbg=14    ctermfg=NONE  cterm=NONE
hi   IncSearch           ctermbg=2     ctermfg=7     cterm=NONE
hi   Search              ctermbg=2     ctermfg=7     cterm=bold
hi   Directory           ctermbg=NONE  ctermfg=4     cterm=NONE
hi   MatchParen          ctermbg=10    ctermfg=NONE  cterm=NONE
hi   SpellBad            ctermbg=NONE  ctermfg=1     cterm=underline
hi   SpellCap            ctermbg=NONE  ctermfg=1     cterm=underline
hi   SpellLocal          ctermbg=NONE  ctermfg=5     cterm=underline
hi   SpellRare           ctermbg=NONE  ctermfg=1     cterm=underline
hi   ModeMsg             ctermbg=NONE  ctermfg=NONE  cterm=bold
hi   MoreMsg             ctermbg=NONE  ctermfg=NONE  cterm=bold
hi   Cursor              ctermbg=NONE  ctermfg=7     cterm=NONE
hi   QuickFixLine        ctermbg=NONE  ctermfg=NONE  cterm=bold
"hi  StatusLineTerm      ctermbg=NONE  ctermfg=NONE  cterm=NONE
"hi  StatusLineTermNC    ctermbg=NONE  ctermfg=NONE  cterm=NONE
"hi  StatusLineTermNC    ctermbg=NONE  ctermfg=NONE  cterm=NONE
hi   qfLineNr            ctermbg=NONE  ctermfg=8     cterm=NONE
"hi  FoldColumn          ctermbg=NONE  ctermfg=NONE  cterm=NONE

hi  link  Number        Constant
hi  link  FoldColumn    LineNr
hi  link  ColorColumn   CursorLine
hi  link  CursorColumn  CursorLine
hi  link  SignColumn    Normal
hi  link  ErrorMsg      Error
hi  link  Question      Normal
hi  link  WarningMsg    Error


" Vim color file
" Maintainer:	koekeishiya
" Last Change:	2017 Jan 31

"set bg&
"hi clear

"if exists("syntax_on")
"  syntax reset
"endif

"let colors_name = "koe"

"" SYNTAX HIGHLIGHT GROUPS
""
"" *Comment    any comment
""
"" *Constant   any constant
""  String     a string constant: "this is a string"
""  Character  a character constant: 'c', '\n'
""  Number     a number constant: 234, 0xff
""  Boolean    a boolean constant: TRUE, false
""  Float      a floating point constant: 2.3e10
""
"" *Identifier any variable name
""  Function   function name (also: methods for classes)
""
"" *Statement        any statement
""  Conditional      if, then, else, endif, switch, etc.
""  Repeat           for, do, while, etc.
""  Label            case, default, etc.
""  Operator         'sizeof', '+', '*', etc.
""  Keyword          any other keyword
""  Exception        try, catch, throw
""
"" *PreProc      generic Preprocessor
""  Include      preprocessor #include
""  Define       preprocessor #define
""  Macro        same as Define
""  PreCondit    preprocessor #if, #else, #endif, etc.
""
"" *Type             int, long, char, etc.
""  StorageClass     static, register, volatile, etc.
""  Structure        struct, union, enum, etc.
""  Typedef          A typedef
""
"" *Special          any special symbol
""  SpecialChar      special character in a constant
""  Tag              you can use CTRL-] on this
""  Delimiter        character that needs attention
""  SpecialComment   special things inside a comment
""  Debug            debugging statements
""
"" *Underlined       text that stands out, HTML links
""
"" *Ignore           left blank, hidden |hl-Ignore|
""
"" *Error            any erroneous construct
""
"" *Todo             anything that needs extra attention;
""                   mostly the keywords TODO FIXME and XXX

"hi Pmenu ctermfg=7 

"hi Normal ctermfg=7
"hi Comment ctermfg=8 

"hi Identifier ctermfg=White guifg=#4f4f4f
"hi cCustomFunc ctermfg=DarkGray guifg=#4f4f4f
"hi link Function cCustomFunc

"hi Statement ctermfg=White guifg=#4f4f4f
"hi Operator ctermfg=White guifg=Green

"hi Type ctermfg=Green guifg=Green
"hi StorageClass ctermfg=White guifg=#4f4f4f
"hi Structure ctermfg=White guifg=Green

"hi Constant ctermfg=Cyan guifg=DarkCyan
"hi String ctermfg=DarkBlue guifg=DarkBlue
"hi Character ctermfg=DarkBlue guifg=DarkBlue
"hi Number ctermfg=Cyan guifg=DarkCyan
"hi Boolean ctermfg=Cyan guifg=Green
"hi Special ctermfg=DarkBlue guifg=DarkBlue

"hi SignColumn ctermbg=Black guibg=Black
"hi lineNr ctermfg=8 guifg=DarkGray

"hi Todo ctermfg=DarkGreen guifg=DarkGreen ctermbg=NONE guibg=NONE cterm=bold gui=bold,underline
"hi myNote ctermfg=DarkGreen ctermbg=NONE cterm=bold
"hi myStatic ctermfg=White

"hi Error ctermfg=Black ctermbg=DarkRed guifg=Black guibg=DarkRed

"hi TabLine ctermbg=Black ctermfg=8 guibg=Black guifg=DarkGray
"hi TabLineSel ctermfg=DarkRed guifg=DarkRed
"hi TabLineFill ctermfg=Black guifg=Black
"hi VertSplit ctermbg=Black ctermfg=Black guifg=Black guibg=Green

"hi Search cterm=bold ctermfg=Black ctermbg=DarkRed guifg=Black gui=bold guibg=DarkRed
"hi Visual ctermfg=Black ctermbg=DarkRed cterm=NONE

"hi PreProc ctermfg=Green guifg=Green
"hi MatchParen cterm=bold ctermbg=NONE ctermfg=Green
"hi multiple_cursors_cursor ctermbg=Green
"hi link multiple_cursors_visual Visual

"hi StatusLine ctermfg=Black ctermbg=8 guifg=Black guibg=DarkGray
"hi StatusLineNC ctermfg=Black ctermbg=8 guifg=Black guibg=DarkGray
"hi CursorLine cterm=NONE ctermbg=Black gui=NONE guibg=Black
"hi StatusLine ctermbg=NONE cterm=NONE
