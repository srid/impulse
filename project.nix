{ system ? builtins.currentSystem
}:
let 
  inherit (import ./dep/gitignore { }) gitignoreSource;
  reflexPlatform = import ./dep/reflex-platform { 
    inherit system;
  };
  project = reflexPlatform.project ({pkgs, ...}: {
    useWarp = true;
    withHoogle = false;
    packages = {
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
