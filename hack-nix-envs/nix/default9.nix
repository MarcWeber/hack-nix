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
       {gte = "1.5.1";  n = "Cabal";}  {n = "process";}  {n = "mtl";}
       {n = "pretty";}
       {i1 = {gte = "2";};  i2 = {lt = "5";};  n = "base";}
       {gte = "1.0";  n = "filepath";}  {n = "mtl";}  {n = "interlude";}
     ];
   }];
  sha256 = "1vrf2g8mg6wg0s57qfnn1sya5lpwdjr531glaww51hz4kq2c5ahh";
  url = "file:///pr/haskell/hack-nix/dist/hack-nix-0.1.1.tar.gz";
}