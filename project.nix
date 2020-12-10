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
    packages = {
      relude = hackGet ./dep/relude;
      rememorate = pkgs.lib.cleanSource (gitignoreSource ./.);
    };
    shells = {
      ghc = ["rememorate"];
      ghcjs = ["rememorate"];
    };
  });
in {
  inherit project reflexPlatform;
}
