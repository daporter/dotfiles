if executable('asciidoctor')
    setlocal makeprg=asciidoctor\ %:S
    " TODO: more error & warning formats need to be added:
    setlocal errorformat=asciidoctor:\ %tARNING:\ %f:\ line\ %l:\ %m
endif
