let nixOverlay = import "/pr/gitnixdev/nixpkgs-haskell-overlay" {};
    lib = nixOverlay.lib;
    pkgs = nixOverlay.pkgs;
    pkgFlags = lib.fold (a: n: a // n) {} (map ({n, v}: lib.attrSingleton n v) [ ]);
    pkg = builtins.getAttr "hack-nix" ((nixOverlay.haskellOverlayPackagesFun.merge (args: args // {
      targetPackages = [{ n = "hack-nix"; v = "99999"; }];
      packageFlags = args.packageFlags // lib.attrSingleton "hack-nix-99999" pkgFlags;
      packages = args.packages ++ [ (nixOverlay.libOverlay.pkgFromDb (import ./default9.nix)) ];
      haskellPackages = pkgs.haskellPackages_ghc703;
      debugS = true;
    }))).result;
in {
      env = nixOverlay.envFromHaskellLibs {
         createHaskellTagsFor = pkg.deps
                              ++ [ (pkgs.haskellPackages_ghc703.ghcPlain // { srcDir = "libraries compiler/main"; })
                                   (pkgs.haskellPackages_ghc703.ghcPlain // { srcDir = "compiler/main"; })
                                 ];
         buildInputs = [ pkgs.haskellPackages_ghc703.ghc ] ++ pkg.buildInputs ++ pkg.deps;
      };
   }
