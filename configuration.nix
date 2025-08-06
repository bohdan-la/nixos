# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, inputs, configDir, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Generation name
  system.nixos.label = "stylix_test_done";

  # For nixd lsp server
  nix.nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
  
  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Use latest kernel.
  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking.hostName = "nixos"; # Define your hostname.

  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/Kyiv";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "uk_UA.UTF-8";
    LC_IDENTIFICATION = "uk_UA.UTF-8";
    LC_MEASUREMENT = "uk_UA.UTF-8";
    LC_MONETARY = "uk_UA.UTF-8";
    LC_NAME = "uk_UA.UTF-8";
    LC_NUMERIC = "uk_UA.UTF-8";
    LC_PAPER = "uk_UA.UTF-8";
    LC_TELEPHONE = "uk_UA.UTF-8";
    LC_TIME = "uk_UA.UTF-8";
  };

  services.xserver = {
    enable = true;

    desktopManager = {
      xterm.enable = false;
    };
   
    windowManager.i3 = {
      enable = true;
      package = pkgs.i3-gaps;
      extraPackages = with pkgs; [
        dmenu #application launcher most people use
        i3status # gives you the default i3 status bar
        i3blocks #if you are planning on using i3blocks over i3status
     ];
    };

    # Configure keymap in X11
    xkb = {
      layout = "us, ua";
      variant = "";
      options = "grp:win_space_toggle";
    };
  };

  services.displayManager.defaultSession = "none+i3";  
  programs.i3lock.enable = true; #default i3 screen locker

  services.logind = {
    lidSwitch = "suspend";
    lidSwitchDocked = "ignore";
    lidSwitchExternalPower = "suspend";
    powerKey = "ignore";
    powerKeyLongPress = "poweroff";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

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
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.bohdan = {
    isNormalUser = true;
    description = "bohdan";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [
    ];
  };

  # Enable automatic login for the user.
  services.displayManager.autoLogin.enable = true;
  services.displayManager.autoLogin.user = "bohdan";

  # Install firefox.
  programs.firefox.enable = true;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    git
    wget
    picom
    feh
    brightnessctl
    pavucontrol
    nixd
    nil
    nixfmt-rfc-style
    logseq
    kitty
   ];

  services.emacs = {
    enable = true;
    package = pkgs.emacs;
  };
    
  programs.vim = {
    enable = true;
    defaultEditor = true;
  };

  fonts.packages = with pkgs; [
    nerd-fonts._0xproto
    nerd-fonts.caskaydia-mono
    nerd-fonts.fantasque-sans-mono
    nerd-fonts.fira-mono
    nerd-fonts.hack
    nerd-fonts.go-mono
    nerd-fonts.intone-mono
    nerd-fonts.monaspace
    nerd-fonts.space-mono
  ];
  
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    # also pass inputs to home-manager modules
    extraSpecialArgs = { inherit inputs; inherit configDir; };
    users = {
      bohdan = import ./home.nix;
    };
  };

  # Setting stylix
  stylix = {
    enable = false;
    
    autoEnable = false;

    image = "${configDir}/wallpapers/solarized_tree.png";

    base16Scheme = "${pkgs.base16-schemes}/share/themes/solarized-dark.yaml";

    fonts = {
      monospace = {
        package = pkgs.fira-code;
        name = "FiraCode Nerd Font Mono";
      };
    };

    # targets.kitty.enable = true;
  };


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
  system.stateVersion = "25.05"; # Did you read the comment?
}
