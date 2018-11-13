CompilerSet makeprg=pytest\ --tb=line\ -q

CompilerSet errorformat=%-G%.%#100%%%.%#
CompilerSet errorformat+=%-G%.%#FAILURES%.%#
CompilerSet errorformat+=%f:%l:\ %m
CompilerSet errorformat+=%-G%.%#in\ %.%#\ seconds
