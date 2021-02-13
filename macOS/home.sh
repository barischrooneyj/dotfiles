#!/bin/zsh

set -euo pipefail

mkdir -p ~/.config/{fish,kitty,nvim} ~/.doom.d
cp ~/dotfiles/doom/config.el         ~/.doom.d/config.el
cp ~/dotfiles/doom/packages.el       ~/.doom.d/packages.el
cp ~/dotfiles/fish/config.fish       ~/.config/fish/config.fish
cp ~/dotfiles/kitty/kitty.conf       ~/.config/kitty/kitty.conf
cp ~/dotfiles/kitty/dracula.conf     ~/.config/kitty/dracula.conf
cp ~/dotfiles/kitty/diff.conf        ~/.config/kitty/diff.conf
cp ~/dotfiles/nvim/init.vim          ~/.config/nvim/init.vim
cp ~/dotfiles/starship/starship.toml ~/.config/starship.toml
cp ~/dotfiles/tmux/.tmux.conf        ~/.tmux.conf
