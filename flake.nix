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
          pkg = self.packages.${system}.${packageName};
        in
          {
            packages.${packageName} = haskellPackages.callCabal2nix
              packageName self rec {};

            defaultPackage = pkg;

            devShell = (pkgs.haskell.lib.doBenchmark pkg).env;
          }
    );
}
