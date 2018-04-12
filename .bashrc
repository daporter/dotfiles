# From https://superuser.com/questions/183870/difference-between-bashrc-and-bash-profile?answertab=active#tab-top
#
# ~/.bashrc is the place to put stuff that applies only to bash itself, such
# as alias and function definitions, shell options, and prompt settings. (You
# could also put key bindings there, but for bash they normally go into
# ~/.inputrc.)
#
# Even better information here:
# https://blog.flowblok.id.au/2013-02/shell-startup-scripts.html
#
# From the Bash documentation:
#   For almost every purpose, shell functions are preferred over aliases.

# From Homebrew bash-completion:
[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion

# From Homebrew bash-git-prompt:
if [ -f "/usr/local/opt/bash-git-prompt/share/gitprompt.sh" ]; then
    __GIT_PROMPT_DIR="/usr/local/opt/bash-git-prompt/share"
    source "/usr/local/opt/bash-git-prompt/share/gitprompt.sh"
fi
