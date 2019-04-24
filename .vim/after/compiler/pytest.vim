CompilerSet makeprg=pytest\ --tb=line\ --quiet

CompilerSet errorformat=
            \%-G%.%#%.%#%%%.%#,
            \%-G%.%#FAILURES%.%#,
            \%f:%l:\ %m,
            \%-G%.%#in\ %.%#\ seconds,
