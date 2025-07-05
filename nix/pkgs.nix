{ inputs, ... }: {
  perSystem = { config, system, compiler, ... }:
    let
      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [
          inputs.iohk-nix.overlays.crypto
          inputs.iohk-nix.overlays.haskell-nix-crypto
          inputs.haskellNix.overlay
          (_final: _prev: {
            cabal-install = pkgs.haskell-nix.tool compiler "cabal-install" "3.10.3.0";
            cabal-fmt = config.treefmt.programs.cabal-fmt.package;
            fourmolu = config.treefmt.programs.fourmolu.package;
            haskell-language-server = pkgs.haskell-nix.tool compiler "haskell-language-server" "2.11.0.0";
            weeder = pkgs.haskell-nix.tool compiler "weeder" "2.9.0";
          })
        ];
      };
    in
    {
      _module.args = { inherit pkgs; };
    };

}
