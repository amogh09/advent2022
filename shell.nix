{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/af8cd5ded7735ca1df1a1174864daab75feeb64a.tar.gz") {}
}:
pkgs.mkShell {
  nativeBuildInputs = [
    (pkgs.haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
      cabal-fmt haskell-language-server cabal-install
    ]))
    pkgs.ormolu
  ];
}
