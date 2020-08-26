{ config, pkgs, ... }:

let
  unstable = import <nixos-unstable> { config = config.nixpkgs.config; };
in {
  # Allow unfree software.
  nixpkgs.config.allowUnfree = true;
  # Garbage collect old generations.
  nix.gc.automatic = true;
  nix.gc.dates = "--delete-older-than 7d";
  # Imports. Include results of the hardware scan.
  imports = [ ./hardware-configuration.nix ];
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  # Networking. Per-interface useDHCP will be mandatory in the future.
  networking.useDHCP = false;
  networking.interfaces.wlp3s0.useDHCP = true;
  networking.hostName = "nixos";
  networking.wireless.enable = true;  # Via wpa_supplicant.
  networking.wireless.networks = {
    "VODAFONE-2.4G" = { psk = "pass"; };
  };
  # Localisation.
  i18n.defaultLocale = "en_IE.UTF-8";
  services.xserver.layout = "gb";
  services.xserver.xkbVariant = "mac";
  services.xserver.xkbOptions = "caps:escape";
  console.useXkbConfig = true;
  time.timeZone = "Europe/Dublin";
  # Packages.
  environment.systemPackages = with pkgs; [
    cava compton curl emacs feh haskellPackages.xmobar git kitty lsd neofetch
    neovim rofi spotifyd starship sublime3 tuir tmux wget wirelesstools xclip

    cabal-install cabal2nix home-manager vlc

    unstable.firefox-devedition-bin-unwrapped unstable.spotify-tui
  ];
  programs.fish.enable = true;
  fonts.fonts = with pkgs; [ fira-code montserrat ];
  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  # X11 windowing system.
  services.xserver.enable = true;
  # Xmonad window manager.
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.enableContribAndExtras = true;
  # Skip the login screen.
  services.xserver.displayManager.defaultSession = "none+xmonad";
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.displayManager.lightdm.autoLogin.enable = true;
  services.xserver.displayManager.lightdm.autoLogin.user = "jeremy";
  # Enable touchpad support.
  services.xserver.libinput.enable = true;
  # User account.
  users.users.jeremy.isNormalUser = true;
  users.users.jeremy.extraGroups = [ "wheel" ];
  users.users.jeremy.shell = pkgs.fish;
  # NixOS version.
  system.stateVersion = "20.03"; # Did you read the comment?
}
