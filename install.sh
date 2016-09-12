set -euo pipefail

# ~/.bash_profile
bash_profile_path=${HOME}/.bash_profile
bash_profile_lines=(
  "alias ls=\"ls -a\""
  "alias vi=\"vim\""
  "export CLICOLOR=1"
  "export HOMEBREW_CASK_OPTS=\"--appdir=/Applications\""
)
for line in "${bash_profile_lines[@]}"; do
  grep -q "$line" "$bash_profile_path" || echo "$line" >> "$bash_profile_path"
done
source "$bash_profile_path"

# install applications
case "$OSTYPE" in
  darwin*)
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)" || true
    brew install ghc haskell-stack node python3 wget
    brew cleanup
    brew cask install Caskroom/versions/sublime-text3 firefox flux \
                      google-chrome google-drive skype transmission vlc
    brew cask cleanup
  ;; 
  linux*)
    echo "TODO install Linux apps"
  ;;
  *)
    echo "unknown: $OSTYPE"
  ;;
esac

# Clone GitHub projects to ~/Documents
while true; do
    read -p $'Enter a GitHub repo to clone e.g. barischj/dotfiles\n' input
    if [[ $input = "" ]]; then break; fi
    user_repo=($(echo $input | tr "/" " "))
    if (( ${#user_repo[@]} != 2 )); then
        echo "Input in incorrect format";
    else
        git clone https://github.com/${user_repo[0]}/${user_repo[1]} \
        ${HOME}/Documents/${user_repo[0]}/${user_repo[1]}
    fi
done
