alias c="clear"
alias ls="ls -ahGF"
alias vi="vim"

export CLICOLOR=1
export LSCOLORS=GxFxBxDxCxegedabagacad
export PATH=$PATH:$HOME/.local/bin

if [ -f $(brew --prefix)/share/bash-completion/bash_completion ]; then
  . $(brew --prefix)/share/bash-completion/bash_completion
fi

export PS1="\e[35;1m\u\e[m in \e[33;1m\w\e[m \nλ "

[[ -z ${TMUX+x} ]] && ( tmux a || tmux )
