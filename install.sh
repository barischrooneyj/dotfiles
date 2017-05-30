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
xcode-select --install ||:
 
# Homebrew
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" ||:

# Homebrew installs
brew install bash bash-completion@2 git-flow-avh haskell-stack mas node python3
brew cask install firefox flux franz google-chrome google-drive iterm2 spotify sublime-text transmission vlc

# Xcode
read -p $'Install Xcode?\n' -n 1
echo
if [[ $REPLY =~ ^[Yy]$ ]]
    mas signin --dialog barischj@tcd.ie ||:
    mas install `mas search Xcode | head -n 1 | cut -f 1 -d ' '`
    sudo xcodebuild -license accept
fi

# Fira Code font
brew tap caskroom/fonts
brew cask install font-fira-code

# Spacemacs
rm -rf ~/.*emacs*
brew tap d12frosted/emacs-plus
brew install emacs-plus
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
## haskell layer
stack install --install-ghc apply-refact hlint stylish-haskell hasktags hoogle intero
## javascript layer
npm install -g eslint js-beautify tern
## python layer
pip install autoflake flake8 hy
## react layer
npm install -g babel-eslint eslint eslint-plugin-react js-beautify tern

# Update config files
filemap=(  # {relative url: absolute path}
    ".bash_profile $HOME/.bash_profile"
    ".bashrc $HOME/.bashrc"
    ".spacemacs $HOME/.spacemacs"
    "Preferences.sublime-settings \
        $HOME/Library/Application\ Support/Sublime\ Text\ 3/Packages/User/Preferences.sublime-settings"
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
brew install duti
duti -s org.videolan.vlc mkv all
duti -s org.videolan.vlc m4a all
duti -s org.videolan.vlc mp4 all

# Cleanup
brew cleanup
brew cask cleanup

# Open apps
open /Applications/Flux.app
open /Applications/Google\ Chrome.app
open /Applications/Google\ Drive.app

# Install all "appropriate" updates
softwareupdate --install --all

# Bug fixes
cat << EOM
For anaconda-mode errors see:
https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Blang/python#auto-completion-anaconda-dependencies
https://github.com/syl20bnr/spacemacs/issues/8412
EOM
