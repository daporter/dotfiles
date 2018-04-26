# Run ls -Al
ll() {
    set -- -Al "$@"
    ls "$@"
}
