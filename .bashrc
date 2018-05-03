# Bash completion.
if [ -f $(brew --prefix)/share/bash-completion/bash_completion ]; then
    . $(brew --prefix)/share/bash-completion/bash_completion
fi

alias c='clear'
alias e='emacsclient -t'
alias l='ls -ahGF'
alias t='tmux a'
alias tk='tmux kill-server'

# If ALTERNATE_EDITOR is the empty string, start Emacs in daemon mode and try
# connecting again.
export ALTERNATE_EDITOR=""
export CLICOLOR=1
export LSCOLORS=GxFxBxDxCxegedabagacad
export PATH=$PATH:$HOME/.local/bin  # Stack binaries.
export PS1="\e[35;1m\u\e[m in \e[33;1m\w\e[m \n>>= "

attach_custom_tmux_session () {
    tmux new-session -d 'emacsclient -t'
    tmux split-window -h -p 38 -c "#{pane_current_path}"
    tmux select-pane -L
    tmux a
}

# Attach if not in tmux (start server if necessary).
[[ -z ${TMUX+x} ]] && ( tmux a || attach_custom_tmux_session )
