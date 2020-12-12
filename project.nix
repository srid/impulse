{ system ? builtins.currentSystem
, withHoogle ? false
}:
let 
  inherit (import ./dep/gitignore { }) gitignoreSource;
  reflexPlatform = import ./dep/reflex-platform { 
    inherit system;
  };
  project = reflexPlatform.project ({pkgs, hackGet ,...}: {
    inherit withHoogle;
    useWarp = true;

    packages = let 
      # neuronPrj = hackGet ./dep/neuron;
      neuronPrj = ../neuron;
      neuronSrc = neuronPrj + "/neuron";
      cm = hackGet ./dep/commonmark;
    in {
      relude = hackGet ./dep/relude;
      rememorate = pkgs.lib.cleanSource (gitignoreSource ./.);

      # neuron & its dependencies (not already in reflex-platform)
      neuron = pkgs.runCommand "neuron" { buildInputs = [ neuronSrc ]; }
        ''
        mkdir $out
        cp -r -p ${neuronSrc}/* $out/
        # Remove non-library stanzas (expected to be at the end)
        head -n -76 $out/neuron.cabal > $out/tmp
        mv $out/tmp $out/neuron.cabal
        '';
      reflex-dom-pandoc = import (neuronPrj + "/dep/reflex-dom-pandoc/thunk.nix");
      pandoc-link-context = import (neuronPrj + "/dep/pandoc-link-context/thunk.nix");
      commonmark = cm + "/commonmark";
      commonmark-pandoc = cm + "/commonmark-pandoc";
      commonmark-extensions = cm + "/commonmark-extensions";
      algebraic-graphs = hackGet ./dep/alga;
      clay = hackGet ./dep/clay;
      dependent-sum-aeson-orphans = hackGet ./dep/dependent-sum-aeson-orphans;
      # neuron requires >= 0.2.5.0
      aeson-gadt-th = hackGet ./dep/aeson-gadt-th;
    };

    overrides = self: super: with pkgs.haskell.lib; {
      neuron = if withHoogle then super.neuron else dontHaddock super.neuron;
      algebraic-graphs = dontCheck super.algebraic-graphs;  # Test fails
    };

    shells = {
      ghc = ["rememorate"];
      ghcjs = ["rememorate"];
    };
  });
in {
  inherit project reflexPlatform;
}
