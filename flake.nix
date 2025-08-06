{
  description = "NixOS config flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";

    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    stylix = {
      url = "github:nix-community/stylix/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, ... }@inputs: 
    # use "nixos", or your hostname as the name of the configuration
    # it's a better practice than "default" shown in the video
    let
      configDir = "/etc/nixos";
    in {
      nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs; inherit configDir; };
        modules = [
          inputs.home-manager.nixosModules.default
          inputs.stylix.nixosModules.stylix
          ./configuration.nix
        ];
      };
    };
}
