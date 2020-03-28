CompilerSet makeprg=pydocstyle

" Look here for tips on writing an errorformat:
" https://flukus.github.io/vim-errorformat-demystified.html

" comparator/plan.py:9 in public method `__init__`:
CompilerSet errorformat=%E%f:%l\ %.%#
"        D107: Missing docstring in __init__
CompilerSet errorformat+=%Z\ %#D%n:\ %m

