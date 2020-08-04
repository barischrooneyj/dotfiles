#!/bin/bash

set -euo pipefail

# Configure dock.
defaults write com.apple.dock orientation -string left
defaults write com.apple.dock persistent-apps -array
# defaults write com.apple.dock autohide -bool true
killall Dock

# Speed up cursor.
defaults write -g KeyRepeat -int 1
defaults write -g ApplePressAndHoldEnabled -bool false

# AppleScript scripts.
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
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
fi
brew tap d12frosted/emacs-plus
brew tap homebrew/cask-fonts
brew install duti emacs@28 pyenv reattach-to-user-namespace tmux
brew cask install firefox-developer-edition flux font-iosevka font-iosevka-slab google-backup-and-sync google-chrome iterm2 signal spotify sublime-text transmission vlc
brew install openssl readline sqlite3 xz zlib # pyenv: https://github.com/pyenv/pyenv/wiki#suggested-build-environment

# Haskell
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Doom Emacs.
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install
brew install fd findutils ripgrep

# Move dotfiles into place.
dotfiles=(
    '.tmux.conf'
    '.zshrc'
)
for filename in "${dotfiles[@]}"; do
    url="https://raw.githubusercontent.com/barischj/dotfiles/master/$filename"
    src_file="$(curl -fsSL $url)"
    echo "Setting $filename from $url"
    dest_path="$HOME/$filename"
    mkdir -p "$(dirname $dest_path)"
    echo "$src_file" > "$dest_path"
done

# Oh My Zsh.
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
git clone https://github.com/reobin/typewritten.git $ZSH_CUSTOM/themes/typewritten
(cd $ZSH_CUSTOM/themes/typewritten && git pull && cd -)
source ~/.zshrc

# Tmux Plugin Manager.
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

# iTerm Dracula theme.
git clone https://github.com/dracula/iterm.git

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

# Iosevka font.
# ~/Library/Fonts

# Cleanup.
brew cleanup

# Open apps that require further action.
open -a 'firefox developer edition'
read -n 1 -s -r -p "Press any key to continue"
open -a 'backup and sync'
read -n 1 -s -r -p "Press any key to continue"
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
softwareupdate --install --all --restart
