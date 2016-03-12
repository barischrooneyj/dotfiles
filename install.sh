#!/bin/bash
set -e

# .bash_profile
cat > "$HOME/.bash_profile" << EOM
export HOMEBREW_CASK_OPTS="--appdir=/Applications"
alias cask="brew cask"
alias vi="vim"
EOM

# brew and cask
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew install python3 node
brew cleanup
cask install Caskroom/versions/sublime-text3 firefox flux google-chrome google-drive skype transmission vlc
cask cleanup
