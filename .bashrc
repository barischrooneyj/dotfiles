alias c="clear"
alias ls="ls -a"
alias vi="vim"
export CLICOLOR=1

if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi

export PATH=$PATH:$HOME/.local/bin
PS1='> '
