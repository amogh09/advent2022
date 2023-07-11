{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = with pkgs; [
    (haskell.packages.ghc927.ghcWithPackages
      (pkgs: with pkgs;
      [ cabal-install haskell-language-server ormolu cabal-fmt eventlog2html ])
    )
  ];
}
