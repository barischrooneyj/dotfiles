export ZSH='/Users/jeremy/.oh-my-zsh'
HIST_STAMPS='dd/mm/yyyy'
plugins=(cabal git osx tmux)
ZSH_THEME='typewritten/typewritten'
export TYPEWRITTEN_CURSOR='block'
export TYPEWRITTEN_SYMBOL='>>='
export TYPEWRITTEN_COLOR_MAPPINGS='secondary:green'
export ZSH_TMUX_AUTOSTART='true'
export ZSH_TMUX_AUTOSTART_ONCE='false'
source $ZSH/oh-my-zsh.sh

alias c='clear'
alias l='ls -A'
alias ll='ls -lhA'
alias gs='git status'
export PATH=~/.emacs.d/bin:$PATH

if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi
