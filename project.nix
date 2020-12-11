{ system ? builtins.currentSystem
}:
let 
  inherit (import ./dep/gitignore { }) gitignoreSource;
  reflexPlatform = import ./dep/reflex-platform { 
    inherit system;
  };
  project = reflexPlatform.project ({pkgs, hackGet ,...}: {
    useWarp = true;
    withHoogle = false;

    packages = let 
      neuronPrj = hackGet ./dep/neuron;
      neuronSrc = neuronPrj + "/neuron";
      cm = hackGet ./dep/commonmark;
    in {
      relude = hackGet ./dep/relude;
      rememorate = pkgs.lib.cleanSource (gitignoreSource ./.);

      # neuron & its dependencies (not already in reflex-platform)
      neuron =  pkgs.runCommand "neuron" { buildInputs = [ neuronSrc ]; }
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
    };

    overrides = self: super: with pkgs.haskell.lib; {
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
