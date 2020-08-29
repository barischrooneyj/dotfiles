{ config, pkgs, ... }:

let
  unstable = import <nixos-unstable> { config = config.nixpkgs.config; };
in {
  # NixOS version.
  system.stateVersion = "20.03";
  # Allow unfree software.
  nixpkgs.config.allowUnfree = true;
  # Garbage collect old generations.
  nix.gc.automatic = true;
  nix.gc.dates = "--delete-older-than 7d";
  ############
  # HARDWARE #
  ############
  # Include results of the hardware scan.
  imports = [ ./hardware-configuration.nix ];
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  # Skip boot messages.
  boot.plymouth.enable = true;
  # Networking. Per-interface useDHCP will be mandatory in the future.
  networking.useDHCP = false;
  networking.interfaces.wlp3s0.useDHCP = true;
  networking.hostName = "nixos";
  networking.wireless.enable = true;  # Via wpa_supplicant.
  networking.wireless.networks = {
    "VODAFONE-2.4G" = { psk = "pass"; };
  };
  # Keyboard.
  services.xserver.layout = "gb";
  services.xserver.xkbVariant = "mac";
  services.xserver.xkbOptions = "caps:escape";
  console.useXkbConfig = true;
  # Trackpad.
  services.xserver.libinput.enable = true;
  services.xserver.libinput.naturalScrolling = true;
  # Sound and bluetooth.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull;
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;
  # Display: colour temperature.
  services.redshift.enable = true;
  location.provider = "geoclue2";
  # Display: backlight.
  programs.light.enable = true;
  services.actkbd.enable = true;
  services.actkbd.bindings = [
    { keys = [ 225 ]; events = [ "key" ]; command = "/run/current-system/sw/bin/light -A 10"; }
    { keys = [ 224 ]; events = [ "key" ]; command = "/run/current-system/sw/bin/light -U 10"; }
  ];
  # Webcam.
  hardware.facetimehd.enable = true;
  # mbpfan fan controller.
  services.mbpfan.enable = true;
  # SSD optimisation.
  services.fstrim.enable = true;
  ###################
  # ACCOUNT & LOGIN #
  ###################
  users.users.jeremy.isNormalUser = true;
  users.users.jeremy.extraGroups = [ "video" "wheel" ];
  users.users.jeremy.shell = pkgs.fish;
  environment.variables.EDITOR = "nvim";
  # Skip the login screen.
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.displayManager.lightdm.autoLogin.enable = true;
  services.xserver.displayManager.lightdm.autoLogin.user = "jeremy";
  #######################
  # DESKTOP ENVIRONMENT #
  #######################
  # X11 windowing system.
  services.xserver.enable = true;
  # Xmonad window manager.
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.enableContribAndExtras = true;
  services.xserver.displayManager.defaultSession = "none+xmonad";
  ############
  # PACKAGES #
  ############
  environment.systemPackages = with pkgs; [
    bitwarden-cli cabal-install cabal2nix cava curl emacs feh
    haskellPackages.xmobar home-manager git kitty lsd neofetch neovim rofi
    spotifyd starship sublime3 tuir tmux vlc wget wirelesstools xclip
    # Unstable packages.
    unstable.firefox-devedition-bin-unwrapped unstable.spotify-tui
  ];
  fonts.fonts = with pkgs; [ fira-code montserrat ];
  programs.fish.enable = true;
  ################
  # LOCALISATION #
  ################
  i18n.defaultLocale = "en_IE.UTF-8";
  time.timeZone = "Europe/Dublin";
}
