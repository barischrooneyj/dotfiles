{ config, pkgs, lib, ... }:

{
  home.file.".config/bg.jpg".source           = "/home/jeremy/dotfiles/background.jpg";
  home.file.".doom.d/config.el".source        = "/home/jeremy/dotfiles/doom/config.el";
  home.file.".doom.d/packages.el".source      = "/home/jeremy/dotfiles/doom/packages.el";
  home.file.".config/nvim/init.vim".source    = "/home/jeremy/dotfiles/nvim/init.vim";
  home.file.".config/kitty/kitty.conf".source = "/home/jeremy/dotfiles/kitty/kitty.conf";
  home.file.".config/kitty/dracula.conf".source = "/home/jeremy/dotfiles/kitty/dracula.conf";
  home.file.".config/kitty/diff.conf".source = "/home/jeremy/dotfiles/kitty/diff.conf";
  home.file.".xmonad/xmonad.hs".source        = "/home/jeremy/dotfiles/xmonad/xmonad.hs";
  home.file.".config/xmobar/xmobar.hs".source = "/home/jeremy/dotfiles/xmobar/xmobar.hs";
  programs.git = {
    enable = true;
    userName = "barischrooneyj";
    userEmail = "barischrooneyj@protonmail.com";
  };
}
