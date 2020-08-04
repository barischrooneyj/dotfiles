DIR=iosevka-dist
git clone https://github.com/ejuarezg/containers $DIR && cd $DIR/iosevka_font
mkdir build_dir && cd build_dir
curl https://raw.githubusercontent.com/barischrooneyj/dotfiles/master/private-build-plans.toml -o private-build-plans.toml
cd ..
docker run -it -v $(pwd)/build_dir:/build iosevka_build
