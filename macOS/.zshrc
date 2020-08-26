# Attach if not in tmux (start server if necessary).
[[ -z ${TMUX+x} ]] && ( tmux new-session -A -s main )

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Zsh plugins.
source ~/antigen.zsh
# From the default repo.
antigen use oh-my-zsh
antigen bundle docker
antigen bundle git
antigen bundle pyenv
antigen bundle tmux
# From the zsh-users repo.
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle zsh-users/zsh-completions
antigen bundle zsh-users/zsh-syntax-highlighting # Must be the last plugin sourced!
antigen bundle zsh-users/zsh-history-substring-search # Must be after zsh-syntax-highlighting!
# Theming and apply.
antigen theme romkatv/powerlevel10k
antigen apply

# vi editing mode.
bindkey -v
bindkey "^R" history-incremental-search-backward

# Environment variables.
export KEYTIMEOUT=1 # 10 ms.
export PATH=~/.emacs.d/bin:$PATH
export PATH=~/.local/bin:$PATH

# Aliases.
alias c='clear'
alias e='emacs -nw'
alias l='ls -A'
alias ll='ls -lhA'
alias gl='git log'
alias gs='git status'
alias vi='nvim'
alias vim='nvim'

# ghcup.
[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env" # ghcup-env

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
