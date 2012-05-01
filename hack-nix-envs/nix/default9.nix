{
  name = "hack-nix";  version="99999";
  edeps = 
  [{
     cdeps = [];
     deps = 
     [
       {n = "haskell98";}  {n = "base";}  {n = "directory";}
       {n = "containers";}  {n = "network";}  {n = "zlib";}
       {n = "tar";}  {n = "bytestring";}  {n = "filepath";}
       {gte = "1.8.0.6";  n = "Cabal";}  {n = "process";}  {n = "mtl";}
       {n = "pretty";}
       {i1 = {gte = "2";};  i2 = {lt = "5";};  n = "base";}
       {gte = "1.0";  n = "filepath";}  {n = "mtl";}  {n = "interlude";}
       {n = "unix";}
     ];
   }];
  sha256 = "0nlmyvjv8nydaghhzjj93pnz70gdcracvq5224f8zkgwg2z84lqp";
  tsdeps = [];
  url = "file:///pr/haskell/hack-nix/dist/hack-nix-0.1.1.tar.gz";
}