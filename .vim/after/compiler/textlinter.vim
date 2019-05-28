" Compiler settings for my own script that aggregates multiple text linting
" programs.

CompilerSet makeprg=~/bin/textlinter\ %

" Look here for tips on writing an errorformat:
" https://flukus.github.io/vim-errorformat-demystified.html

" proselint:
" /tmp/email.txt:3:44: dates_times.am_pm.spacing It's standard to put a space before 'a.m.' or 'p.m.'.
CompilerSet errorformat=%f:%l:%c:\ %*[^\ ]\ %m

" writegood:
" In /tmp/email.txt
" =============
" The girl was hit.
"          ^^^^^^^
" "was hit" may be passive voice on line 8 at column 9
CompilerSet errorformat+=%EIn\ %f
CompilerSet errorformat+=%Z%m\ on\ line\ %l\ at\ column\ %c
CompilerSet errorformat+=%C%.%#
