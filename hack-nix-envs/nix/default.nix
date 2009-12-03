let nixOverlay = import "/pr/gitnixdev/haskell-nix-overlay" {};
    lib = nixOverlay.lib;
    pkgs = nixOverlay.pkgs;
    pkgFlags = lib.fold (a: n: a // n) {} (map ({n, v}: lib.attrSingleton n v) [ ]);
    pkg = builtins.getAttr "hack-nix" (nixOverlay.haskellOverlayPackagesFun.merge (args: args // {
      targetPackages = [{ n = "hack-nix"; v = "99999"; }];
      packageFlags = args.packageFlags // lib.attrSingleton "hack-nix-99999" pkgFlags;
      packages = args.packages ++ [ (nixOverlay.libOverlay.pkgFromDb (import ./default9.nix)) ];
      haskellPackages = pkgs.haskellPackages;
      debugS = true;
    })).result;
in {
      env = nixOverlay.envFromHaskellLibs {
         createHaskellTagsFor = pkg.propagatedBuildInputs
                              ++ [ (pkgs.haskellPackages.ghcReal // { srcDir = "libraries compiler/main"; })
                                   (pkgs.haskellPackages.ghcReal // { srcDir = "compiler/main"; })
                                 ];
         buildInputs = pkg.buildInputs ++ pkg.propagatedBuildInputs;
      };
   }
