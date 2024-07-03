{
  description = "Serialization framework for OCaml";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    rio = {
      url = "github:riot-ml/rio";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    minttea = {
      url = "github:leostera/minttea";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
      perSystem = { config, self', inputs', pkgs, system, ... }:
        let
          inherit (pkgs) ocamlPackages mkShell;
          inherit (ocamlPackages) buildDunePackage;
          version = "0.0.1+dev";
        in
          {
            devShells = {
              default = mkShell {
                buildInputs = [ ocamlPackages.utop ];
                inputsFrom = [
                  self'.packages.serde_derive
                  self'.packages.default
                  self'.packages.serde_json
                ];
              };
            };

            packages = {
              default = buildDunePackage {
                inherit version;
                pname = "serde";
                propagatedBuildInputs = with ocamlPackages; [
                  qcheck
                  inputs'.rio.packages.default
                  inputs'.minttea.packages.spices
                ];
                src = ./.;
              };
              serde_derive = buildDunePackage {
                inherit version;
                pname = "serde_derive";
                propagatedBuildInputs = with ocamlPackages; [
                  self'.packages.default
                  ppx_deriving
                  ppxlib
                ];
                src = ./.;
              };
              serde_json = buildDunePackage {
                inherit version;
                pname = "serde_json";
                propagatedBuildInputs = with ocamlPackages; [
                  self'.packages.default
                  self'.packages.serde_derive
                  ppx_inline_test
                  qcheck
                  inputs'.rio.packages.default
                  inputs'.minttea.packages.spices
                  yojson
                ];
                src = ./.;
              };
            };
          };
    };
}
