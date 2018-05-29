
export PS1="\[\033[36m\]\u\[\033[m\]@\[\033[32m\]\h:\[\033[33;1m\]\w\[\033[m\]\$ "
export CLICOLOR=1
export LSCOLORS=ExFxBxDxCxegedabagacad
alias ls='ls -GFh'

# OPAM configuration
. /Users/kosgei/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

# added by Anaconda3 4.4.0 installer
# export PATH="/Users/kosgei/anaconda/bin:$PATH"

export PATH="$HOME/.cargo/bin:$PATH"

# load environment variables from a config file
source /Users/kosgei/projects/bin/env_vars.sh

