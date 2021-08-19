{
  description = "Circular stacks";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (
      system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          haskellPackages = pkgs.haskellPackages;
          packageName = "circular";
        in
          {
            packages.${packageName} = haskellPackages.callCabal2nix
              packageName self rec {};

            defaultPackage = self.packages.${system}.${packageName};

            devShell = pkgs.mkShell {
              buildInputs = with haskellPackages; [
                haskell-language-server
                stack
              ];
              inputsFrom = builtins.attrValues self.packages.${system};
            };
          }
    );
}
