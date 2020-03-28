" CompilerSet makeprg=pytest\ --tb=line\ --quiet
CompilerSet makeprg=pytest

" Note that this requires the following to be set:
" export PYTEST_ADDOPTS="--tb=line"

" /plan.py:152: TypeError: 'TariffBlock' object does not support indexing
CompilerSet errorformat=%f:%l:\ %m
CompilerSet errorformat+=%-G%.%#
