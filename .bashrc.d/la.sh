# Run ls --almost-all (exclude "." and "..") 
la() {
    set -- -A "$@"
    ls "$@"
}
