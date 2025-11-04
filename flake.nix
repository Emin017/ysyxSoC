{
  description = "rocket-chip";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs-old.url = "github:NixOS/nixpkgs/fa9a51752f1b5de583ad5213eb621be071806663";
  };

  outputs = { self, nixpkgs, flake-utils, nixpkgs-old }@inputs:
    let
      overlay = import ./overlay.nix;
    in
    flake-utils.lib.eachDefaultSystem
      (system:
      let
        pkgs = import nixpkgs {
            inherit system;
            overlays = [ overlay ];
            config.packageOverrides = pkgs: {
              circt = nixpkgs-old.legacyPackages.${system}.circt;
            };
        };
        deps = with pkgs; [
          mill
          circt
        ];
      in
        {
          legacyPackages = pkgs;
          devShell = pkgs.mkShell {
            buildInputs = deps;
            shellHook = ''
                export CHISEL_FIRTOOL_PATH="${pkgs.circt}/bin"
            '';
          };
        }
      )
    // { inherit inputs; overlays.default = overlay; };
}

