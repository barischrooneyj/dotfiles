{ config, pkgs, ... }:

let
  unstable = import <nixos-unstable> { config = config.nixpkgs.config; };
  pinentry-gtk-2 = pkgs.symlinkJoin {
    name = "pinentry-gtk-2";
    paths = [ pkgs.pinentry-gtk2 ];
    buildInputs = [ pkgs.makeWrapper ];
  };

in {
  # NixOS version.
  system.stateVersion = "20.09";
  # Allow unfree software.
  nixpkgs.config.allowUnfree = true;
  # Garbage collect old generations.
  nix.gc.automatic = true;
  nix.gc.dates = "--delete-older-than 7d";
  ############
  # HARDWARE #
  ############
  # Include results of the hardware scan.
  imports = [ ./hardware-configuration.nix ./cachix.nix ];
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  # Intel microcode.
  hardware.cpu.intel.updateMicrocode = true;
  # Icon instead of boot messages.
  boot.plymouth.enable = true;
  # Networking. Per-interface useDHCP will be mandatory in the future.
  networking.useDHCP = false;
  networking.interfaces.wlp3s0.useDHCP = true;
  networking.wireless.enable = true; # Via wpa_supplicant.
  networking.hostName = "nixos"; # Default hostname.
  # Keyboard layout.
  services.xserver.layout = "gb";
  services.xserver.xkbVariant = "mac";
  console.useXkbConfig = true;
  # Faster key repeat.
  services.xserver.autoRepeatDelay = 300;
  services.xserver.autoRepeatInterval = 25;
  # Trackpad.
  services.xserver.libinput.enable = true;
  services.xserver.libinput.naturalScrolling = true;
  # Sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull;
  # Bluetooth.
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;
  # Webcam.
  hardware.facetimehd.enable = true; # Mac specific.
  # Fan controller.
  services.mbpfan.enable = true; # Mac specific.
  # SSD optimisation.
  services.fstrim.enable = true;
  # OpenGL.
  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;
  #################
  # FUNCTION KEYS #
  #################
  services.actkbd.enable = true;
  # Display backlight.
  programs.light.enable = true;
  services.actkbd.bindings = [
    { keys = [ 225 ]; events = [ "key" ]; command = "/run/current-system/sw/bin/light -A 10"; }
    { keys = [ 224 ]; events = [ "key" ]; command = "/run/current-system/sw/bin/light -U 10"; }
  ];
  # Sound.
  sound.mediaKeys.enable = true;
  ###################
  # SYSTEM PACKAGES #
  ###################
  environment.systemPackages = with pkgs; [
    # Stable packages.
    acpi arandr arc-theme asciiquarium betterlockscreen bitwarden-cli blender
    cabal-install cabal2nix cava cinnamon.nemo cmatrix curl espeak feh git
    google-chrome haskellPackages.xmobar home-manager insync kitty lsd neofetch
    neovim nix-prefetch-git nodePackages.webtorrent-cli papirus-icon-theme
    pavucontrol pinentry-gtk-2 ranger rofi signal-desktop sl spotify spotifyd
    starship sublime3 texlive.combined.scheme-full toilet tomb transmission-gtk
    tree tuir tmux vlc wget wirelesstools xclip tomb zip zoom-us
    direnv
    # Unstable packages.
    unstable.brave unstable.emacs unstable.firefox-devedition-bin-unwrapped
    unstable.steam unstable.spotify-tui
  ];
  services.lorri.enable = true;
  fonts.fonts = with pkgs; [
    fira-code
    montserrat
    (unstable.nerdfonts.override { fonts = [ "FiraCode" ]; })
  ];
  virtualisation.docker.enable = true;
  programs.fish.enable = true;
  #################
  # LOGIN MANAGER #
  #################
  services.xserver.displayManager.gdm.enable = true;
  # services.xserver.displayManager.lightdm.greeters.gdm.enable = true;
  # services.xserver.displayManager.lightdm.greeters.gtk.indicators =
  #   [ "~spacer" "~clock" "~session" "~a11y" "~power" ];
  # services.xserver.displayManager.lightdm.greeters.gtk.extraConfig = ''
  #   active-monitor=1
  #   background=#zoomed:/etc/lightdm/bg.jpg
  # '';
  services.xserver.displayManager.defaultSession = "none+xmonad";
  services.xserver.displayManager.sessionCommands = ''
    /usr/share/display-setup.sh
  '';
  ######################
  # DESKTOP & GRAPHICS #
  ######################
  # XMonad.
  services.xserver.enable = true;
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.enableContribAndExtras = true;
  # Wallpaper.
  services.xserver.desktopManager.wallpaper.mode = "fill";
  # Redshift enabled by default.
  services.redshift.enable = true;
  location.provider = "geoclue2";
  # VSync.
  services.xserver.videoDrivers = [ "intel" ];
  services.xserver.deviceSection = ''
    Option "DRI"      "2"
    Option "TearFree" "true"
  '';
  # Composer.
  services.picom.enable = true;
  ########
  # USER #
  ########
  users.users.jeremy.isNormalUser = true;
  users.users.jeremy.extraGroups = [ "docker" "video" "wheel" ];
  users.users.jeremy.shell = pkgs.fish;
  #################
  # USER SETTINGS #
  #################
  environment.variables.EDITOR = "nvim";
  # Localisation.
  i18n.defaultLocale = "en_IE.UTF-8";
  time.timeZone = "Europe/Dublin";
  # Wireless networks.
  networking.wireless.networks = {
   "ssid"   = { psk = "psk"; };
  };
  # Rebind keys.
  services.xserver.xkbOptions = "caps:escape";
  # Binary cache settings for Tontine.
  nix.useSandbox = true;
  nix.binaryCaches = [ "https://cache.nixos.org/" "https://hydra.iohk.io/" ];
  nix.binaryCachePublicKeys = [ "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
}
