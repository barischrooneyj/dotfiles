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
brew install bash bash-completion git haskell-stack koekeishiya/khd/khd kwm node python3

# GUI apps
brew cask install atom docker firefox flux google-chrome google-drive skype sublime-text transmission vlc

# Fira Code font
brew tap caskroom/fonts
brew cask install font-fira-code

# Spacemacs
rm -rf ~/.*emacs*
brew tap railwaycat/emacsmacport
brew cask install emacs-mac
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
stack install apply-refact hlint stylish-haskell hasktags hoogle intero

# mkdirs for configs
mkdir "$HOME/.kwm/kwmrc"
# Update config files
filemap=(  # {relative url: absolute path}
    ".bash_profile $HOME/.bash_profile"
    ".bashrc $HOME/.bashrc"
    ".spacemacs $HOME/.spacemacs"
    "Preferences.sublime-settings \
        $HOME/Library/Application\ Support/Sublime\ Text\ 3/Packages/User/Preferences.sublime-settings"
    ".khdrc $HOME/.khdrc"
    ".kwm/kwmrc $HOME/.kwm/kwmrc"
)
for line in "${filemap[@]}"; do
    read src_path dest_path <<< $line
    url="https://raw.githubusercontent.com/barischj/dotfiles/master/$src_path"
    src_file="$(curl -fsSL $url)"
    echo "Setting $dest_path from $url"
    mkdir -p "$(dirname "$dest_path")"
    echo "$src_file" > "$dest_path"
done

# Source before we run things
source ~/.bashrc

# Set paths
launchctl setenv PATH $PATH

# Start services
brew services start khd kwh

# Clone GitHub repos
msg='Enter GitHub repo to clone e.g. barischj/dotfiles or return to continue: '
while true; do
    read -p "$msg" input
    if [[ "$input" = "" ]]; then break; fi
    user_repo=($(echo "$input" | tr "/" " "))
    if (( ${#user_repo[@]} != 2 )); then
        echo "Input format incorrect";
    else
        git clone "https://github.com/${user_repo[0]}/${user_repo[1]}" \
            "${HOME}/Documents/${user_repo[0]}_${user_repo[1]}" ||:
    fi
done

# Open apps
open /Applications/Flux.app
open /Applications/Google\ Drive.app

# Change shell to bash 4
usr_bash='/usr/local/bin/bash'
shells='/etc/shells'
grep -qF $usr_bash $shells || \
    ( echo "Appending $usr_bash to $shells" && \
      echo $usr_bash | sudo tee -a $shells )
grep -qF $usr_bash <<< $SHELL || chsh -s $usr_bash
echo "Open new shell for updated bash"
