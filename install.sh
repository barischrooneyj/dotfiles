#!/bin/bash

set -eou pipefail

# Configure dock
defaults write com.apple.dock autohide -bool true
defaults write com.apple.dock orientation -string left
defaults delete com.apple.dock persistent-apps ||:
killall Dock

# Speed up cursor
defaults write -g KeyRepeat -int 1
defaults write -g ApplePressAndHoldEnabled -bool false

# Xcode command line tools
# TODO: Possibly covered by installing brew
xcode-select --install ||:
 
# Homebrew
if hash brew 2>/dev/null; then
  echo 'brew already installed'
else
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi
brew tap caskroom/fonts
brew tap caskroom/homebrew-versions
brew install bash bash-completion@2 duti emacs git-flow-avh haskell-stack ispell python3 tmux
brew cask install font-fira-code google-backup-and-sync google-chrome iina iterm2-beta spotify transmission
brew cleanup
brew cask cleanup

# Spacemacs
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d ||:
## haskell layer deps
stack install --install-ghc apply-refact hlint stylish-haskell hasktags hoogle intero
## python layer deps
sudo pip3 install autoflake flake8 hy

# Set dotfiles
filemap=(
    ".bash_profile"
    ".bashrc"
    ".spacemacs"
    ".tmux.conf"
)
for name in "${filemap[@]}"; do
    url="https://raw.githubusercontent.com/barischj/dotfiles/master/$name"
    src_file="$(curl -fsSL $url)"
    echo "Setting $name from $url"
    dest_path="$HOME/$name"
    mkdir -p "$(dirname "$dest_path")"
    echo "$src_file" > "$dest_path"
done

# Set default apps
defaults=(
    "avi com.colliderli.iina"
    "m4a com.colliderli.iina"
    "mkv com.colliderli.iina"
    "mp4 com.colliderli.iina"
)
for line in "${defaults[@]}"; do
    read format app <<< $line
    duti -s $app $format all
done

# Open apps
open -a backup\ and\ sync
open -a flux

# Final steps
echo 'Remaining issues: https://github.com/barischrooneyj/dotfiles/issues'

# Install "all appropriate" updates
softwareupdate --install --all
