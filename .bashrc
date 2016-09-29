alias c="clear"
alias ls="ls -a"
alias vi="vim"
export CLICOLOR=1

if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi

alias docker-yesod-devel="stack --docker-run-args='--net=bridge --publish=3000:3000' exec -- yesod devel"

