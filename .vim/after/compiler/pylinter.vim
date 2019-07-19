" Compiler settings for my own script that aggregates multiple Python linting
" programs.

CompilerSet makeprg=~/bin/pylinter

" Look here for tips on writing an errorformat:
" https://flukus.github.io/vim-errorformat-demystified.html

" darglint
" comparator/plan.py:14 I201: - return
CompilerSet errorformat=%f:%l\ %t%n:\ %m

" pylint:
CompilerSet errorformat+=%-G*%#\ Module\ %.%#
" comparator/plan.py:18:4: C0103: Argument name "d" doesn't conform to snake_case naming style (invalid-name)
CompilerSet errorformat+=%f:%l:%c:\ %t%n:\ %m

" pydocstyle:
" comparator/plan.py:9 in public method `__init__`:
CompilerSet errorformat+=%W%f:%l\ %.%#
"        D107: Missing docstring in __init__
CompilerSet errorformat+=%Z\ %#%t%n\ %m

" CompilerSet errorformat+=%-G%.%#
