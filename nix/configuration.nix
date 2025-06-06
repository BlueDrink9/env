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
in { lib, config, pkgs, ... }:
{

  # TODO: Change this
  networking.hostName = lib.mkDefault "placeholder_hostname"; # change in /etc/nixos/flake.nix too

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

  imports =
    [
      # "/home/w/env/system/nix/packages.nix"
      # "${NIX_CONFIG_DIR}/packages.nix"
      ./packages/all.nix
    ];

  nixpkgs.config.allowUnfree = true;

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.configurationLimit = 4;
  # boot.loader.systemd-boot.rebootForBitlocker = true; # experimental
  boot.loader.timeout = 0;

  # boot.plymouth.logo
  # boot.plymouth.theme

  # Enable magic sysrq key
  boot.kernel.sysctl."kernel.sysrq" = 1;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;
  # networking.networkmanager.wifi.powersave = true;
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = lib.mkForce "Pacific/Auckland";

  # Select internationalisation properties.
  i18n.defaultLocale = lib.mkForce "en_NZ.UTF-8";

  i18n.extraLocaleSettings = lib.mkForce {
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
  services.xserver.xkb = lib.mkForce {
    layout = "us,nz";
    # DisplayNames = "co,qw"; not actually an option
    variant = "colemak,";
    options = "grp:win_space_toggle";
  };
  console.useXkbConfig = true;
  # console.keyMap = "colemak";

  users.defaultUserShell = pkgs.zsh;

  nix.settings.experimental-features = "nix-command flakes";

  # Enable CUPS to print documents.
  services.printing.enable = lib.mkDefault true;

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.enp2s0.useDHCP = lib.mkDefault true;
  # networking.interfaces.wlp3s0.useDHCP = lib.mkDefault true;
  
  # Explicitly enable your ethernet interface
  networking.interfaces.enp0s31f6.useDHCP = true;
  
  # Add a systemd service to restart NetworkManager on boot
  # This helps with issues where NetworkManager starts before all interfaces are ready
  systemd.services.restart-network-manager = {
    description = "Restart NetworkManager On Boot";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.systemd}/bin/systemctl restart NetworkManager";
      RemainAfterExit = true;
    };
  };

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  # Enable sound with pipewire.
  services.pulseaudio.enable = false;
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
  system.stateVersion = lib.mkForce "24.05"; # Did you read the comment?

  # Perform garbage collection weekly to maintain low disk usage
  nix.gc = {
    automatic = true;
    # Trigger if last cue was missed
    persistent = true;
    dates = lib.mkDefault "weekly";
    options = lib.mkDefault "--delete-older-than 2w";
  };
  nix.settings.auto-optimise-store = true;

  security.sudo.execWheelOnly = true;
  security.sudo.extraConfig = ''
    Defaults        timestamp_timeout=60
  '';

  # Disable wakeup from PCI devices
  services.udev.extraRules = ''
  ACTION=="add", SUBSYSTEM=="pci", DRIVER=="pcieport", ATTR{power/wakeup}="disabled"
  '';

  services.logind = {
    lidSwitch="suspend";
    # lidSwitch="suspend-then-hibernate";
    lidSwitchDocked="ignore";
    lidSwitchExternalPower="ignore";
    # suspendKey="suspend-then-hibernate";
    suspendKey="suspend";
    powerKey="poweroff";
    extraConfig = ''
  #   IdleAction=suspend-then-hibernate
  #   IdleActionSec=10m
  #   HibernateDelaySec=65m
    '';

  };
  systemd.sleep.extraConfig = ''
        HibernateDelaySec=60min
  '';

  systemd.extraConfig = ''
    DefaultTimeoutStopSec=30s
  '';

  services.thermald.enable = true;
  services.auto-cpufreq.enable = true;
  # Conflicts with auto-cpufreq; install just the package for kde etc to use it.
  services.power-profiles-daemon.enable = false;

  # Trim SSDs
  services.fstrim.enable = true;

  # Limit maximum size of journalctl logs
  # Runtime is in-memory logs under /run, System is disk logs.
  services.journald.extraConfig = ''
    [Journal]
    Compress=yes
    SystemMaxUse=1G
    SystemKeepFree=1G
    RuntimeMaxUse=100M
  '';


  # Better virtual terminal in console
  services.kmscon = {
    # TMP: https://github.com/NixOS/nixpkgs/issues/385497
    enable = false;
    hwRender = true;
    # fonts = [ {
    #   name = "MesloLGLDZ Nerd Font";
    #   package = (pkgs.nerdfonts.override { fonts = [ "menlo" ]; });
    # } ];
    useXkbConfig = true;
    extraConfig = ''
      font-size=11
      palette=solarized
    '';
  };

  boot.crashDump.enable = true;
  boot.kernel.sysctl."kernel.panic_on_oops" = 1;

  environment.shellAliases = {
    renix="sudo nixos-rebuild switch --impure";
  };

  services = {
    syncthing = {
      openDefaultPorts = true;
      settings = {
        options = {
          minDiskFree = {value = 1; unit = "%";};
          minHomeDiskFree = {value = 5; unit = "%";};
          maxFolderConcurrency = 1;
          reconnectionIntervalS = 120;
          progressUpdateIntervalS = 60;
          urAccepted = -1;
        };
      };
    };
  };

  # Bind backlight keys. May not be right keys for other than asus k501L
  programs.light = {
    enable = true;
    brightnessKeys.enable = true;
  };

   # services.actkbd = {
   #   enable = true;
   #   bindings = [
   #     # Need system path for the /run/wrappers/bin/light SUID wrapper.
   #     { keys = [ 223 ]; events = [ "key" ]; command = "${pkgs.light}/bin/light -A 10"; }
   #     { keys = [ 224 ]; events = [ "key" ]; command = "${pkgs.light}/bin/light -U 10"; }
   #   ];
   # };

}
