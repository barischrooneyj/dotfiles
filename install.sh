# .bash_profile
bash_profile="$HOME/.bash_profile"
touch "$bash_profile"
echo "export HOMEBREW_CASK_OPTS=\"--appdir=/Applications\"" >> "$bash_profile"
echo "alias cask=\"brew cask\"" >> "$bash_profile"
echo "alias vi=\"vim\"" >> "$bash_profile"

# brew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew install python3
cask install google-chrome google-drive firefox vlc transmission flux Caskroom/versions/sublime-text3
cask cleanup
