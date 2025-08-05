{ config, pkgs, lib, configDir, ... }:

let
  dotfilesDir = "${configDir}/dotfiles";

  dotfiles = [
    "kitty/kitty.conf"
    "kitty/current-theme.conf"
    "i3/config"
    "i3status/config"
    "picom/picom.conf"
    "emacs/init.el"
    "emacs/custom.el"
    "wallpapers/solarized_tree.png"
  ];
in {
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "bohdan";
  home.homeDirectory = "/home/bohdan";

  # symlinking dotfiles
  home.activation.linkDotfiles = lib.hm.dag.entryAfter ["writeBoundary"] ''
    echo "Symlinking dotfiles..."

    ${lib.concatStringsSep "\n" (map (path: ''
      mkdir -p ~/.config/${lib.removeSuffix (baseNameOf path) path}
      ln -sf ${dotfilesDir}/${path} ~/.config/${path}
    '') dotfiles)}
  '';

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "25.05"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = [
    # # Adds the 'hello' command to your environment. It prints a friendly
    # # "Hello, world!" when run.
    # pkgs.hello

    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
  ];

  programs.git = {
    enable = true;
    userName = "bohdan-la";
    userEmail = "bohdan_la@knu.ua";
    extraConfig.init.defaultBranch = "main";
  };

  programs.bash = {
    enable = true;
    shellAliases = {
      ll = "ls -alF";
      sysbuild = "sudo nixos-rebuild switch";
    };
  };

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';

    # symlinking my dotfiles
    # ".config" = {
    #   source = config.lib.file.mkOutOfStoreSymlink "./dotfiles";
    #   recursive = true;
    #   force = true;
    # };
  };

  # xdg.configFile = pkgs.lib.genAttrs dotfiles (path: {
  #   source = config.lib.file.mkOutOfStoreSymlink "${dotfilesDir}/${path}";
  #   force = true;
  # });

  # # alternative way to symlink dotfiles
  # let
  #   mk = config.lib.file.mkOutOfStoreSymlink;
  #   dotfile = path: mk "./dotfiles/${path}";
  # in {
  #   xdg.configFile = {
  #     "kitty/kitty.conf".source = dotfile "kitty/kitty.conf";
  #     "i3/config".source = dotfile "i3/config";
  #     "i3status/config".source = dotfile "i3status/config";
  #     "picom/picom.conf".source = dotfile "picom/picom.conf";
  #     "emacs/init.el".source = dotfile "emacs/init.el";
  #     "emacs/custom.el".source = dotfile "emacs/custom.el";
  #   };
  # }


  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. These will be explicitly sourced when using a
  # shell provided by Home Manager. If you don't want to manage your shell
  # through Home Manager then you have to manually source 'hm-session-vars.sh'
  # located at either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  ~/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/bohdan/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
