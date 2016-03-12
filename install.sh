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

# github projects
OIFS=$IFS
IFS=/
for i in gmassiac/project joeoh/gitmystats; do
	user_repo=($i)
	git clone https://github.com/${user_repo[0]}/${user_repo[1]} "$HOME/Documents/${user_repo[1]}"
done
IFS=$OIFS
