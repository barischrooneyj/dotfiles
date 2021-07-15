{ config, pkgs, lib, ... }:

{
  home.file.".tmux.conf".source                 = "/home/jeremy/dotfiles/tmux/.tmux.conf";
  home.file.".config/bg.jpg".source             = "/home/jeremy/dotfiles/background.jpg";
  home.file.".direnvrc".source                  = "/home/jeremy/dotfiles/direnv/.direnvrc";
  home.file.".doom.d/config.el".source          = "/home/jeremy/dotfiles/doom/config.el";
  home.file.".doom.d/packages.el".source        = "/home/jeremy/dotfiles/doom/packages.el";
  home.file.".config/fish/config.fish".source   = "/home/jeremy/dotfiles/fish/config.fish";
  home.file.".config/kitty/kitty.conf".source   = "/home/jeremy/dotfiles/kitty/kitty.conf";
  home.file.".config/kitty/dracula.conf".source = "/home/jeremy/dotfiles/kitty/dracula.conf";
  home.file.".config/kitty/diff.conf".source    = "/home/jeremy/dotfiles/kitty/diff.conf";
  home.file.".config/nvim/init.vim".source      = "/home/jeremy/dotfiles/nvim/init.vim";
  home.file.".config/starship.toml".source      = "/home/jeremy/dotfiles/starship/starship.toml";
  home.file.".config/xmobar/xmobar.hs".source   = "/home/jeremy/dotfiles/xmobar/xmobar.hs";
  home.file.".xmonad/xmonad.hs".source          = "/home/jeremy/dotfiles/xmonad/xmonad.hs";
  programs.git = {
    enable    = true;
    userName  = "jerbaroo";
    userEmail = "jerbaroo.work@pm.me";
  };
  manual.manpages.enable = false; # Why?
}
