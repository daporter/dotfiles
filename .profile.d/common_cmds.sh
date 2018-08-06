common_cmds() {
    pattern="$1"
    if [ -z "$pattern" ]; then
        history | cut -c 8- | sort -b | less
    else
        history | cut -c 8- | grep "$pattern" | sort -b | less
    fi
}
