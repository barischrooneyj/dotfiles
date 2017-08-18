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
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" ||:
brew install bash bash-completion@2 duti emacs git-flow-avh haskell-stack ispell python3 tmux
brew cask install google-backup-and-sync google-chrome iina iterm2 spotify transmission
brew cleanup
brew cask cleanup

# Fira Code font
brew tap caskroom/fonts
brew cask install font-fira-code

# Spacemacs
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
## haskell layer deps
stack install --install-ghc apply-refact hlint stylish-haskell hasktags hoogle intero
## python layer deps
pip install autoflake flake8 hy

# Update config files
filemap=(  # {relative url: absolute path}
    ".bash_profile $HOME/.bash_profile"
    ".bashrc $HOME/.bashrc"
    ".spacemacs $HOME/.spacemacs"
)
for line in "${filemap[@]}"; do
    read src_path dest_path <<< $line
    url="https://raw.githubusercontent.com/barischj/dotfiles/master/$src_path"
    src_file="$(curl -fsSL $url)"
    echo "Setting $dest_path from $url"
    mkdir -p "$(dirname "$dest_path")"
    echo "$src_file" > "$dest_path"
done

# Change default apps
defaults=(
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
