# .bash_profile

# Get the aliases and functions
[ -f $HOME/.bashrc ] && . $HOME/.bashrc

# Load environment variables
export $(envsubst < ~/.config/environment.d/10-basic.conf)
