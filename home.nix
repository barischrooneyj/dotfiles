{ config, pkgs, lib, ... }:

{
  programs.git = {
    enable = true;
    userName = "barischrooneyj";
    userEmail = "barischrooneyj@protonmail.com";
  };

  home.file.".xmonad/xmonad.hs".source = "/home/jeremy/dotfiles/.xmonad/xmonad.hs";

  programs.tmux = {
    enable = true;
    keyMode = "vi";
  };
}

