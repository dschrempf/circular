{
  description = "Circular stacks";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/haskell-updates";

  outputs =
    { self
    , flake-utils
    , nixpkgs
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        hpkgs = pkgs.haskell.packages.ghc921;
        circular = hpkgs.callCabal2nix "circular" self rec { };
        circular-dev = pkgs.haskell.lib.doBenchmark circular;
      in
      {
        packages.circular = circular;

        defaultPackage = circular;

        devShell = hpkgs.shellFor {
          packages = _: [ circular-dev ];
          buildInputs = with pkgs; [
            bashInteractive
            hpkgs.cabal-install
            hpkgs.haskell-language-server
          ];
          doBenchmark = true;
          # withHoogle = true;
        };
      }
    );
}
