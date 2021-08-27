{
  description = "Circular stacks";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (
      system:
        let
          packageName = "circular";
          pkgs = import nixpkgs { inherit system; };
          haskellPackages = pkgs.haskellPackages;
          circular = self.packages.${system}.${packageName};
          circular-dev = pkgs.haskell.lib.doBenchmark circular;
        in
          {
            packages.${packageName} = haskellPackages.callCabal2nix
              packageName self rec {};

            defaultPackage = circular;

            devShell = pkgs.haskellPackages.shellFor {
              packages = _: [ circular-dev ];
              buildInputs = with pkgs; [
                haskellPackages.cabal-install
                haskellPackages.haskell-language-server
              ];
            };
          }
    );
}
