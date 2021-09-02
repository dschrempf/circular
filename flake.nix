{
  description = "Circular stacks";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    { self
    , flake-utils
    , nixpkgs
    }:
      flake-utils.lib.eachDefaultSystem (
        system:
          let
            pkgs = import nixpkgs { inherit system; };
            haskellPackages = pkgs.haskellPackages;
            circular = haskellPackages.callCabal2nix "circular" self rec {};
            circular-dev = pkgs.haskell.lib.doBenchmark circular;
          in
            {
              packages.circular = circular;

              defaultPackage = circular;

              devShell = pkgs.haskellPackages.shellFor {
                packages = _: [ circular-dev ];
                buildInputs = with pkgs; [
                  bashInteractive
                  haskellPackages.cabal-install
                  haskellPackages.haskell-language-server
                  haskellPackages.stack
                ];
                doBenchmark = true;
                withHoogle = true;
              };
            }
      );
}
