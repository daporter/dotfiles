" Compiler settings for my own script that aggregates multiple Python linting
" programs.

CompilerSet makeprg=~/bin/pylinter

" Look here for tips on writing an errorformat:
" https://flukus.github.io/vim-errorformat-demystified.html

" pylint:
CompilerSet errorformat=%-G*%#\ Module\ %.%#
" comparator/plan.py:18:4: C0103: Argument name "d" doesn't conform to snake_case naming style (invalid-name)
CompilerSet errorformat+=%f:%l:%c:\ %t%n:\ %m
"
" flake8:
" tests/unit/test_get_comparison.py:10:1: F401 '.context.comparator' imported but unused
CompilerSet errorformat+=%f:%l:%c:\ %t%n\ %m

" pydocstyle:
" comparator/plan.py:9 in public method `__init__`:
CompilerSet errorformat+=%W%f:%l\ %.%#
"        D107: Missing docstring in __init__
CompilerSet errorformat+=%Z\ %#%t%n:\ %m

" bandit:
" tokenize.py:592: B105: LOW: Possible hardcoded password: '.'
CompilerSet errorformat+=%f:%l:\ %t%n:\ %m
CompilerSet errorformat+=%-G%.%#
