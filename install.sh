set -eou pipefail

# Install applications with Homebrew
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" ||:
brew install bash ghc haskell-stack node python3 wget
brew cleanup
brew cask install firefox flux google-chrome google-drive skype sublime-text transmission vlc
brew cask cleanup

# Update RC files
filemap=(
    "barischj/dotfiles/master/.bashrc $HOME/.bashrc"
)

for line in "${filemap[@]}"; do
    read src_path dest_path <<< $line
    src_file="`wget -qO- https://raw.githubusercontent.com/$src_path`"
    IFS=$'\n' read -rd '' -a src_lines <<< "$src_file"
    touch "$dest_path"
    for src_line in "${src_lines[@]}"; do
        grep -q "$src_line" "$dest_path" || echo "$src_line" >> "$dest_path"
    done
done

source "$HOME/.bashrc"

# Clone GitHub projects to ~/Documents
while true; do
    read -p $'Enter a GitHub repo to clone e.g. barischj/dotfiles\n' input
    if [[ $input = "" ]]; then break; fi
    user_repo=($(echo $input | tr "/" " "))
    if (( ${#user_repo[@]} != 2 )); then
        echo "Input format incorrect";
    else
        git clone https://github.com/${user_repo[0]}/${user_repo[1]} \
        ${HOME}/Documents/${user_repo[0]}_${user_repo[1]}
    fi
done

# Change shell to bash 4.
sudo bash -c 'echo /usr/local/bin/bash >> /etc/shells'
chsh -s /usr/local/bin/bash
