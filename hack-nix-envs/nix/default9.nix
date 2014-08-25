{ name = "hack-nix";  version="99999";  bench_deps = [];
  edeps = 
  [{ cdeps = [];
     deps = 
     [ {n = "base";}  {n = "directory";}  {n = "containers";}
       {n = "network";}  {n = "zlib";}  {n = "tar";}
       {n = "bytestring";}  {n = "filepath";}
       {gte = "1.8.0.6";  n = "Cabal";}  {n = "process";}  {n = "mtl";}
       {n = "pretty";}
       {i1 = {gte = "2";};  i2 = {lt = "5";};  n = "base";}
       {gte = "1.0";  n = "filepath";}  {n = "mtl";}  {n = "interlude";}
       {n = "unix";}
     ];
     tools = [];
   }];
  srcFile = null;  tsdeps = [];
}