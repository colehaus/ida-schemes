let
  extras =
    import ./nix/extras.nix //
    import ./nix/gitignore.nix { inherit (pkgs) lib; };
  pkgs = extras.pinnedPkgs {
    specFile = ./nix/nixpkgs.json;
    opts = { config = { packageOverrides = import ./nix/package-overrides.nix; }; };
  };
in
  pkgs.mkShell {
    inputsFrom = [ (import ./default.nix) ];
    buildInputs = [ pkgs.cabal-install ];
  }
