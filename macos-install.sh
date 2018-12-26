#!/bin/bash

set -euo pipefail

# Configure dock.
# defaults write com.apple.dock autohide -bool true
defaults write com.apple.dock orientation -string left
defaults write com.apple.dock persistent-apps -array
killall Dock

# Speed up cursor.
defaults write -g KeyRepeat -int 1
defaults write -g ApplePressAndHoldEnabled -bool false

# Set system settings with AppleScript.
applescripts=(
    'highlight-colour.scpt'
)
for filename in "${applescripts[@]}"; do
    url="https://raw.githubusercontent.com/barischj/dotfiles/master/applescript/$filename"
    src_file="$(curl -fsSL $url)"
    echo "Running $filename"
    echo "$src_file" > temp.scpt
    osascript temp.scpt
done
rm temp.scpt

# Install apps with Homebrew.
if hash brew 2>/dev/null; then
  echo 'brew is already installed'
else
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi
brew tap caskroom/fonts
brew install aspell bash bash-completion@2 duti haskell-stack python3 reattach-to-user-namespace tmux
brew cask install flux font-fira-code google-backup-and-sync google-chrome iterm2 spotify sublime-text transmission vlc

# Install Spacemacs.
brew tap d12frosted/emacs-plus
brew install emacs-plus
rm -rf ~/.emacs.d && git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
(cd ~/.emacs.d && git checkout develop)
# Haskell layer dependencies.
stack install --resolver lts-12.25 apply-refact hlint stylish-haskell hasktags hoogle

# Move dotfiles into place.
dotfiles=(
    '.bash_profile'
    '.bashrc'
    '.spacemacs'
    '.tmux.conf'
)
for filename in "${dotfiles[@]}"; do
    url="https://raw.githubusercontent.com/barischj/dotfiles/master/$filename"
    src_file="$(curl -fsSL $url)"
    echo "Setting $filename from $url"
    dest_path="$HOME/$filename"
    mkdir -p "$(dirname $dest_path)"
    echo "$src_file" > "$dest_path"
done

# Set default apps.
defaults=(
    "avi org.videolan.vlc"
    "m4a org.videolan.vlc"
    "mkv org.videolan.vlc"
    "mp4 org.videolan.vlc"
)
for line in "${defaults[@]}"; do
    read extension app <<< "$line"
    echo "Setting $app as default for $extension extension"
    duti -s "$app" "$extension" all
done

# Cleanup.
brew cleanup

# Open apps that require further action.
open -a 'backup and sync'
open -a emacs
open -a flux

# Final instructions.

cat << EOM

|-----------------------------------------------------------------------|
|                                                                       |
| * Remaining issues: https://github.com/barischrooneyj/dotfiles/issues |
|                                                                       |
| * Reboot once updates are installed for changes to take effect.       |
|                                                                       |
|-----------------------------------------------------------------------|

EOM

# Install "all appropriate" updates.
softwareupdate --install --all
