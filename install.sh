set -euv

# .bash_profile
bash_profile=${HOME}/.bash_profile 
cat > $bash_profile << EOM
alias cask="brew cask"
alias ls="ls -a"
alias vi="vim"
export CLICOLOR=1
export HOMEBREW_CASK_OPTS="--appdir=/Applications"
EOM
source $bash_profile

# install applications
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew install node python3 wget
brew cleanup
cask install Caskroom/versions/sublime-text3 firefox flux google-chrome google-drive skype transmission vlc
cask cleanup

# github projects
OIFS=$IFS
IFS=/
for user_repo in barischj/spin; do
  git clone https://github.com/${user_repo[0]}/${user_repo[1]} ${HOME}/Documents/${user_repo[1]}
done
IFS=$OIFS
