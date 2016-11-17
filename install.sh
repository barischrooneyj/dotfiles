set -eou pipefail

######
# OS #
######

# Configure dock
defaults write com.apple.dock autohide -bool true
defaults write com.apple.dock orientation -string left
defaults delete com.apple.dock persistent-apps ||:
killall Dock

# Speed up cursor
defaults write -g KeyRepeat -int 1
defaults write -g ApplePressAndHoldEnabled -bool false

############
# INSTALLS #
############

# Install Homebrew
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" ||:

# Install command line apps
brew install bash bash-completion git haskell-stack node python3

# Install GUI apps
brew cask install atom docker firefox flux google-chrome google-drive skype sublime-text transmission vlc

# Install Fira Code font
brew tap caskroom/fonts
brew cask install font-fira-code

# Xcode tools
xcode-select --install ||:

#############
# CONFIGURE #
#############

# ATOM
stack install ghc-mod hlint stylish-haskell
apm install \
    autocomplete-haskell haskell-ghc-mod haskell-pointfree ide-haskell ide-haskell-cabal \
    ide-haskell-repl language-haskell  \
    ask-stack atom-beautify atom-material-ui autocomplete-paths file-icons git-plus minimap \
    minimap-find-and-replace minimap-highlight-selected pigments run-command script \
    sublime-style-column-selection vim-mode
# Config file set below

# Update config files
filemap=(  # {relative url: absolute path}
    "atom_config.cson $HOME/.atom/config.cson"
    ".bash_profile $HOME/.bash_profile"
    ".bashrc $HOME/.bashrc"
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

############
# RUN LAST #
############

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
