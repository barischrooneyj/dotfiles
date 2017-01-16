alias c="clear"
alias ls="ls -ahGF"
alias vi="vim"

export CLICOLOR=1
export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx
export PATH=$PATH:$HOME/.local/bin
export PS1="𝝺 "

if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi
