# From Homebrew bash-git-prompt:
if [ -f "/usr/local/opt/bash-git-prompt/share/gitprompt.sh" ]; then
    __GIT_PROMPT_DIR="/usr/local/opt/bash-git-prompt/share"
    source "/usr/local/opt/bash-git-prompt/share/gitprompt.sh"
fi

function prompt_callback {
    if [ $(jobs | wc -l) -ne 0 ]; then
        echo -n " ${Green}jobs:\j"
    fi
}
