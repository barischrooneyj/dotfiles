# Bash completion.
if [ -f $(brew --prefix)/share/bash-completion/bash_completion ]; then
    . $(brew --prefix)/share/bash-completion/bash_completion
fi

alias c='clear'
alias l='ls -ahGF'
alias tk='tmux kill-server'

export CLICOLOR=1
export LSCOLORS=GxFxBxDxCxegedabagacad
export PATH=$PATH:$HOME/.local/bin  # Stack binaries.
export PS1="\e[35;1m\u\e[m in \e[33;1m\w\e[m \n>>= "

# Attach if not in tmux (start server if necessary).
[[ -z ${TMUX+x} ]] && ( tmux new-session -A -s main )
