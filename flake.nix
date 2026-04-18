{
  description = "CLI controller for sing-box";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      perSystem = nixpkgs.lib.genAttrs systems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          haskellPackages = pkgs.haskellPackages;
          boxctl = haskellPackages.callCabal2nix "boxctl" self { };
          boxctlApp = {
            type = "app";
            program = "${boxctl}/bin/boxctl";
          };
        in
        {
          packages = {
            default = boxctl;
            boxctl = boxctl;
          };
          apps = {
            default = boxctlApp;
            boxctl = boxctlApp;
          };
          devShells = {
            default = haskellPackages.shellFor {
              packages = _: [ boxctl ];
              nativeBuildInputs = [
                pkgs.nil
                pkgs.nixd
                pkgs.cabal-install
                haskellPackages.haskell-language-server
                haskellPackages.hlint
                haskellPackages.cabal-gild
              ];
              shellHook = ''
                export CABAL_DIR="$PWD/.cabal"
                export XDG_CONFIG_HOME="$PWD/.xdg/config"
                export XDG_CACHE_HOME="$PWD/.xdg/cache"
                export XDG_STATE_HOME="$PWD/.xdg/state"
                export XDG_DATA_HOME="$PWD/.xdg/data"
                export PATH="$PATH:$CABAL_DIR/bin"
              '';
            };
          };
        }
      );
    in
    {
      packages = nixpkgs.lib.mapAttrs (_: outputs: outputs.packages) perSystem;
      apps = nixpkgs.lib.mapAttrs (_: outputs: outputs.apps) perSystem;
      devShells = nixpkgs.lib.mapAttrs (_: outputs: outputs.devShells) perSystem;
    };
}
