{ withHoogle ? false, ...}:

(import ./project.nix {inherit withHoogle; }).project.shells.ghc
