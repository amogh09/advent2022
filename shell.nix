{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    (haskellPackages.ghcWithPackages
      (pkgs: with pkgs;
      [ cabal-install haskell-language-server ormolu cabal-fmt eventlog2html ])
    )
  ];
  shellHook = ''
    export PATH="$PATH:$HOME/.local/bin"
    export TIMEFMT='%J   %U  user %S system %P cpu %*E total'$'\n'\
      'avg shared (code):         %X KB'$'\n'\
      'avg unshared (data/stack): %D KB'$'\n'\
      'total (sum):               %K KB'$'\n'\
      'max memory:                %M '$MAX_MEMORY_UNITS''$'\n'\
      'page faults from disk:     %F'$'\n'\
      'other page faults:         %R'
  '';
}
