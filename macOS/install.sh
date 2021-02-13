#!/bin/zsh

set -euo pipefail

##### Copy Config Files #####
./home.sh

##### Install Brew #####
if hash brew 2>/dev/null; then
  echo 'Homebrew already installed'
else
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
fi

##### Dock #####
# defaults write com.apple.dock orientation -string left
defaults write com.apple.dock persistent-apps -array
# defaults write com.apple.dock autohide -bool true
killall Dock

##### Speed up cursor #####
defaults write -g KeyRepeat -int 1
defaults write -g ApplePressAndHoldEnabled -bool false

##### AppleScript #####
osascript applescript/security-privacy.scpt >/dev/null
echo 'Allow Terminal to control your computer\n  System Preferences > Privacy > Accessibility'
read -s -k '?Press enter to continue'$'\n'
echo 'Allow Terminal to control your computer\n  System Preferences > Privacy > Automation'
read -s -k '?Press enter to continue'$'\n'
osascript applescript/appearance.scpt

##### Fish #####
brew install fish
sudo sh -c 'echo /usr/local/bin/fish >> /etc/shells'
chsh -s /usr/local/bin/fish

##### Bass #####
curl -L https://get.oh-my.fish | fish
omf install bass || true

##### Doom Emacs #####
brew tap d12frosted/emacs-plus && brew install emacs-plus --with-no-titlebar
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d || true
~/.emacs.d/bin/doom install
brew install fd findutils ripgrep  # https://github.com/hlissner/doom-emacs#prerequisites

##### pyenv #####
# https://github.com/pyenv/pyenv/wiki#suggested-build-environment
brew install pyenv openssl readline sqlite3 xz zlib

##### Tmux #####
brew install tmux
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm || true

##### Remaining Brew Installs #####
brew install docker duti lsd neovim starship tmux tree
brew install --cask brave-browser discord docker flux kitty mactex-no-gui\
  protonvpn signal spotify sublime-text transmission vlc\
  homebrew/cask-versions/firefox-developer-edition
brew install homebrew/cask-fonts/font-fira-code-nerd-font

##### Nix & Cachix ##### 
sh <(curl -L https://nixos.org/nix/install) --darwin-use-unencrypted-nix-store-volume
nix-env -iA cachix -f https://cachix.org/api/v1/install

##### Install "all appropriate" updates #####
softwareupdate --install --all --restart
