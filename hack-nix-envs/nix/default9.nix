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
       {n = "hslogger";}  {n = "pretty";}
       {i1 = {gte = "2";};  i2 = {lt = "5";};  n = "base";}
       {i1 = {gte = "1.6";};  i2 = {lt = "1.7";};  n = "Cabal";}
       {gte = "1.0";  n = "filepath";}
       {i1 = {gte = "1";};  i2 = {lt = "3";};  n = "network";}
       {i1 = {gte = "4000.0.2";};  i2 = {lt = "4001";};  n = "HTTP";}
     ];
   }];
  srcFile = "/pr/haskell/hack-nix/dist/hack-nix-0.1.1.tar.gz";
}