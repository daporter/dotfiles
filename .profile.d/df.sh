df() {
    /bin/df -hT apfs                     |  # only look at apfs filesystems
        awk '{print $2, $3, $4, $5, $9}' |  # ignore uninsteresting columns
        column -t
}
