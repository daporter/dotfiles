" Compiler settings for my own script that aggregates multiple text linting
" programs.

CompilerSet makeprg=~/bin/textlinter

" Look here for tips on writing an errorformat:
" https://flukus.github.io/vim-errorformat-demystified.html

" proselint:
" /tmp/email.txt:3:44: dates_times.am_pm.spacing It's standard to put a space before 'a.m.' or 'p.m.'.
CompilerSet errorformat=%f:%l:%c:\ %*[^\ ]\ %m

" writegood:
/tmp/message.txt:7:138:"are implemented" may be passive voice
CompilerSet errorformat=%f:%l:%c:%m
