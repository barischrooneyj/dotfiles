export ZSH="/Users/jeremy/.oh-my-zsh"
ENABLE_CORRECTION="true"
HIST_STAMPS="dd/mm/yyyy"
plugins=(git osx)
ZSH_THEME="typewritten/typewritten"
export TYPEWRITTEN_CURSOR="block"
export TYPEWRITTEN_PROMPT_LAYOUT="single_line"
source $ZSH/oh-my-zsh.sh

alias c='clear'
alias gs='git status'
alias tk='tmux kill-server'

# Attach if not in tmux (start server if necessary).
[[ -z ${TMUX+x} ]] && ( tmux new-session -A -s main )
