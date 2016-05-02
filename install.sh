#!/bin/bash
set -euv

# .bash_profile
cat > ${HOME}/.bash_profile << EOM
export CLICOLOR=1
alias cask="brew cask"
alias ls="ls -a"
alias vi="vim"
EOM
source ${HOME}/.bash_profile

# install applications
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew install python3 node
brew cleanup
cask install Caskroom/versions/sublime-text3 firefox flux google-chrome google-drive homebrew/science/octave skype transmission vlc
cask cleanup

# github projects
OIFS=$IFS
IFS=/
for i in barischj/spin; do
  user_repo=($i)
  git clone https://github.com/${user_repo[0]}/${user_repo[1]} ${HOME}/Documents/${user_repo[1]}
done
IFS=$OIFS
