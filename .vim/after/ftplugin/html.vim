setlocal ts=2 sts=2 sw=2 expandtab

if executable('tidy')
    setlocal formatprg=tidy\ --indent\ yes\ --wrap\ 0\ --tidy-mark\ no\ --force-output\ true\ -quiet\ --show-errors\ 0\ --show-warnings\ 0
endif
