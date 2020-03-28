CompilerSet makeprg=python\ -m\ py_compile

"   File "comparator/plan.py", line 10
"     class Plan:
"         ^
" SyntaxError: invalid syntax
 
CompilerSet errorformat=%E%.%#File\ \"%f\"\\,\ line\ %l
CompilerSet errorformat+=%-C%p^
CompilerSet errorformat+=%-C\ \ %.%#
CompilerSet errorformat+=%Z%m
CompilerSet errorformat+=%-G%.%#
