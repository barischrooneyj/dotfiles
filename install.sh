#!/bin/bash
set -euv

# .bash_profile
BASH_PROFILE = "$HOME/.bash_profile"
cat > $BASH_PROFILE << EOM
export CLICOLOR=1
export HOMEBREW_CASK_OPTS="--appdir=/Applications"
alias cask="brew cask"
alias ls="ls -a"
alias vi="vim"
EOM
source $BASH_PROFILE

# install applications
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew install python3 node
brew cleanup
cask install Caskroom/versions/sublime-text3 firefox flux google-chrome google-drive skype transmission vlc
cask cleanup

# github projects
OIFS=$IFS
IFS=/
for i in gmassiac/project joeoh/gitmystats; do
  user_repo=($i)
  git clone https://github.com/${user_repo[0]}/${user_repo[1]} "$HOME/Documents/${user_repo[1]}"
done
IFS=$OIFS
