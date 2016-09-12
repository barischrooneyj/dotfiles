set -eou pipefail

# Install applications with Homebrew
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" ||:
brew install bash ghc haskell-stack node python3 wget
brew cleanup
brew cask install firefox flux google-chrome google-drive skype sublime-text transmission vlc
brew cask cleanup

# Update config files
filemap=(
    "barischj/dotfiles/master/.bashrc $HOME/.bashrc"
    "barischj/dotfiles/master/Preferences.sublime-settings \
        $HOME/Library/Application\ Support/Sublime\ Text\ 3/Packages/User/Preferences.sublime-settings"
)
for line in "${filemap[@]}"; do
    read src_path dest_path <<< $line
    src_file="`wget -qO- https://raw.githubusercontent.com/$src_path`"
    echo "$src_file" > "$dest_path"
done
source "$HOME/.bashrc"

# Clone GitHub repos
msg='Enter GitHub repo to clone e.g. barischj/dotfiles or return to continue: '
while true; do
    read -p "$msg" input
    if [[ $input = "" ]]; then break; fi
    user_repo=($(echo $input | tr "/" " "))
    if (( ${#user_repo[@]} != 2 )); then
        echo "Input format incorrect";
    else
        git clone https://github.com/${user_repo[0]}/${user_repo[1]} \
        ${HOME}/Documents/${user_repo[0]}_${user_repo[1]}
    fi
done

# Change shell to bash 4
sudo bash -c 'echo /usr/local/bin/bash >> /etc/shells'
chsh -s /usr/local/bin/bash
