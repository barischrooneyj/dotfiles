#!/bin/zsh

set -euo pipefail

##### Install Homebrew #####
if hash brew 2>/dev/null; then
  echo '\n** Homebrew already installed **\n'
else
  echo '\n** Installing Homebrew **\n'
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
fi

#### brew install ####
echo '\n** Installing terminal apps with brew **\n'
brew install\
  direnv\
  duti\
  fish\
  git\
  homebrew/cask-fonts/font-fira-code-nerd-font\
  homebrew/cask-fonts/font-iosevka\
  homebrew/cask-fonts/font-iosevka-nerd-font\
  lsd\
  neovim\
  starship\
  tree\
  tmux

#### Set git config ####
git config --global user.name "jerbaroo"
git config --global user.email jerbaroo.work@pm.me

#### brew install --cask ####
echo '\n** Installing GUI apps with brew **\n'
brew install --cask\
  bitwarden\
  brave-browser\
  discord\
  docker\
  flux\
  homebrew/cask-versions/firefox-developer-edition\
  kitty\
  mactex-no-gui\
  protonvpn\
  signal\
  spotify\
  sublime-text\
  transmission\
  vlc
brew cleanup

#### Connect to VPN ####
echo '\n** Manually connect to VPN **\n'
open -a protonvpn
open -a bitwarden
read -s -k '?Press enter to continue'$'\n'

##### Copy Config Files #####
echo '\n** Cloning config files **\n'
git clone https://github.com/jerbaroo/dotfiles temp-dotfiles
./temp-dotfiles/macOS/home.sh
rm -rf temp-dotfiles

##### Dock #####
# defaults write com.apple.dock orientation -string left
echo '\n** Removing apps from the Dock **\n'
defaults write com.apple.dock persistent-apps -array
# defaults write com.apple.dock autohide -bool true
killall Dock

##### Key repeat #####
echo '\n** Setting fast key repeat **\n'
defaults write -g KeyRepeat -int 1
echo '\n** Disabling press and hold key **\n'
defaults write -g ApplePressAndHoldEnabled -bool false

##### fish shell #####
echo '\n** Switching to the fish shell **\n'
sudo sh -c 'echo /usr/local/bin/fish >> /etc/shells'
chsh -s /usr/local/bin/fish

##### Doom Emacs #####
echo '\n** Installing our favourite text editor **\n'
brew tap d12frosted/emacs-plus && brew install emacs-plus --with-no-titlebar
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d || true
echo 'n' | ~/.emacs.d/bin/doom install
brew install fd findutils ripgrep  # https://github.com/hlissner/doom-emacs#prerequisites

##### TPM #####
echo '\n** Installing tmux package manager **\n'
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm || true

##### Bass #####
curl -L https://get.oh-my.fish | fish
omf install bass || true

##### Nix & Cachix ##### 
echo '\n** Installing Nix and Cachix **\n'
sh <(curl -L https://nixos.org/nix/install) --darwin-use-unencrypted-nix-store-volume
. /Users/jer/.nix-profile/etc/profile.d/nix.sh
nix-env -iA cachix -f https://cachix.org/api/v1/install

##### Run AppleScripts #####
# osascript applescript/security-privacy.scpt >/dev/null
# echo 'Allow Terminal to control your computer\n  System Preferences > Privacy > Accessibility'
# read -s -k '?Press enter to continue'$'\n'
# echo 'Allow Terminal to control your computer\n  System Preferences > Privacy > Automation'
# read -s -k '?Press enter to continue'$'\n'
# osascript applescript/appearance.scpt

##### Install "all appropriate" updates #####
sudo softwareupdate --install --all --restart
