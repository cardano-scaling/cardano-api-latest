_: {

  perSystem = { pkgs, hsPkgs, ... }:
    let
      haskellNixShell = hsPkgs.shellFor {
        buildInputs = with pkgs; [
          cabal-install
        ];
      };
    in
    {
      devShells = {
        default = haskellNixShell;
      };
    };
}
