set -eou pipefail

# Configure dock
defaults write com.apple.dock autohide -bool true
defaults write com.apple.dock orientation -string left
defaults delete com.apple.dock persistent-apps ||:
killall Dock

# Speed up cursor
defaults write -g KeyRepeat -int 1
defaults write -g ApplePressAndHoldEnabled -bool false

# Xcode tools
xcode-select --install ||:

# Homebrew
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" ||:

# CLI apps
brew install bash bash-completion git haskell-stack node python3

# GUI apps
brew cask install docker firefox flux google-chrome google-drive iterm2 skype sublime-text transmission vlc

# Fira Code font
brew tap caskroom/fonts
brew cask install font-fira-code

# Spacemacs
rm -rf ~/.*emacs*
brew tap d12frosted/emacs-plus
brew install emacs-plus
brew linkapps emacs-plus
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
stack setup
stack install apply-refact hlint stylish-haskell hasktags hoogle intero

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

# Change shell to bash 4
usr_bash='/usr/local/bin/bash'
shells='/etc/shells'
grep -qF $usr_bash $shells || \
    ( echo "Appending $usr_bash to $shells" && \
      echo $usr_bash | sudo tee -a $shells )
grep -qF $usr_bash <<< $SHELL || \
    ( chsh -s $usr_bash && \
      echo "Open new shell for updated bash" )

# Open apps
open /Applications/Flux.app
open /Applications/Google\ Drive.app

source ~/.bashrc
echo "Log out and in for some changes to take effect"
