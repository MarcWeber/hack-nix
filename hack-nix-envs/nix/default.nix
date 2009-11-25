let nixOverlay = import "/pr/gitnixdev/haskell-nix-overlay" {};
    lib = nixOverlay.lib;
    pkg = builtins.getAttr "hack-nix" (nixOverlay.haskellOverlayPackagesFun.merge (args: args // {
      targetPackages = [{ n = "hack-nix"; v = "99999"; }];
      packageFlags = lib.attrSingleton "hack-nix-99999" {  };
      packages = args.packages ++ [ (nixOverlay.libOverlay.pkgFromDb (import ./default9.nix)) ];
      debugS = true;
    })).result;
in { env = nixOverlay.envFromHaskellLibs (pkg.buildInputs ++ pkg.propagatedBuildInputs); }
