{ config, pkgs, lib, ... }:

{
  home.file.".xmonad/xmonad.hs".source = "/home/jeremy/dotfiles/xmonad/xmonad.hs";
  home.file.".config/xmobar/xmobar.hs".source = "/home/jeremy/dotfiles/xmobar/xmobar.hs";
  programs.git = {
    enable = true;
    userName = "barischrooneyj";
    userEmail = "barischrooneyj@protonmail.com";
  };
}
