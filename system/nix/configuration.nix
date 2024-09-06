# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
# Run:
# ln $DOTFILES_DIR/system/nix/configuration.nix /etc/nixos/configuration.nix
# renix(){sudo DOTFILES_DIR="$DOTFILES_DIR" nixos-rebuild switch}; export -f renix; renix
# # Or just: nixos-rebuild build; sudo nixos-rebuild switch

let
  dotfilesDir = builtins.getEnv "DOTFILES_DIR";
  nixDir = "${dotfilesDir}/system/nix";
in { config, pkgs, ... }:
{

  # TODO: Change this
  networking.hostName = "placeholder_hostname"; # change in /etc/nixos/flake.nix too

  environment.sessionVariables = rec {
    XDG_CACHE_HOME  = "$HOME/.cache";
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_DATA_HOME   = "$HOME/.local/share";
    XDG_STATE_HOME  = "$HOME/.local/state";
    NIX_CONFIG_DIR  = "$HOME/env/system/nix";

    # Not officially in the specification
    # XDG_BIN_HOME    = "$HOME/.local/bin";
    # PATH = [
    # "${XDG_BIN_HOME}"
    # ];
  };

  # Assumes this file is symlinked into /etc/nixos, where the hardware-configuration is
  imports =
    [ # Include the results of the hardware scan.
      # ./hardware-configuration.nix
      # "/home/w/env/system/nix/packages.nix"
      # "${NIX_CONFIG_DIR}/packages.nix"
      ./packages/all.nix
    ];


  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.configurationLimit = 10;
  # boot.loader.systemd-boot.rebootForBitlocker = true; # experimental
  boot.loader.timeout = 0;

  # boot.plymouth.logo
  # boot.plymouth.theme


  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "Pacific/Auckland";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_NZ.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_NZ.UTF-8";
    LC_IDENTIFICATION = "en_NZ.UTF-8";
    LC_MEASUREMENT = "en_NZ.UTF-8";
    LC_MONETARY = "en_NZ.UTF-8";
    LC_NAME = "en_NZ.UTF-8";
    LC_NUMERIC = "en_NZ.UTF-8";
    LC_PAPER = "en_NZ.UTF-8";
    LC_TELEPHONE = "en_NZ.UTF-8";
    LC_TIME = "en_NZ.UTF-8";
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us,nz";
    variant = "colemak,";
    options = "grp:win_space_toggle";
  };
  console.useXkbConfig = true;
  # console.keyMap = "colemak";

  users.defaultUserShell = pkgs.zsh;

  nix.settings.experimental-features = "nix-command flakes";

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  services.libinput.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?

  # nix.gc = { automatic = true; persistent = true; dates = "05:00:00"; options = "--delete-older-than 7d"; };

  # Perform garbage collection weekly to maintain low disk usage
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 2w";
  };

  # nix.settings.auto-optimise-store = true;

  security.sudo.execWheelOnly = true;
  security.sudo.extraConfig = ''
    Defaults        timestamp_timeout=60
  '';

  # Disable wakeup from PCI devices
  services.udev.extraRules = ''
  ACTION=="add", SUBSYSTEM=="pci", DRIVER=="pcieport", ATTR{power/wakeup}="disabled"
  '';

  services.logind = {
    lidSwitch="suspend-then-hibernate";
    lidSwitchDocked="ignore";
    lidSwitchExternalPower="ignore";
    suspendKey="suspend-then-hibernate";
    powerKey="poweroff";
    extraConfig = ''
  #   IdleAction=suspend-then-hibernate
  #   IdleActionSec=10m
  #   HibernateDelaySec=65m
    '';

  };

  environment.etc = {
    rc.local = {
      text = ''
        alias renix="sudo nixos-rebuild switch"
        '';
    };
  };


}
