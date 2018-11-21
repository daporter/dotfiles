CompilerSet makeprg=pytest\ --tb=line\ --quiet

CompilerSet errorformat=%-G%.%#%.%#%%%.%#
CompilerSet errorformat+=%-G%.%#FAILURES%.%#
CompilerSet errorformat+=%f:%l:\ %m
CompilerSet errorformat+=%-G%.%#in\ %.%#\ seconds
