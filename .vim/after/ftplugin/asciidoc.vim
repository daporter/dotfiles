if executable('asciidoctor')
    let &l:makeprg = 'asciidoctor %:S'
    " TODO: more error & warning formats need to be added:
    let &l:errorformat = 'asciidoctor: %tARNING: %f: line %l: %m'
endif
