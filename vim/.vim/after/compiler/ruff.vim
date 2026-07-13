" Compiler settings for ruff, which replaces the pylint/flake8/darglint chain.

CompilerSet makeprg=ruff\ check\ --output-format=concise\ %

" Look here for tips on writing an errorformat:
" https://flukus.github.io/vim-errorformat-demystified.html

" compress_pdf:52:5: F841 Local variable `unused` is assigned to but never used
CompilerSet errorformat=%f:%l:%c:\ %m
