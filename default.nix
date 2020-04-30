{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs { }
, nix-hs ? import sources.nix-hs { inherit pkgs; }
, ghcide ? sources.ghcide-nix
, ormolu ? sources.ormolu
, ghc ? "default"
}:

nix-hs {
  cabal = ./addy.cabal;
  compiler = ghc;

  overrides = lib: self: super: with lib; {
    ghcide = import ghcide { ghc = compilerName; };

    ormolu = (import ormolu {
      inherit (lib) pkgs;
      ormoluCompiler = lib.compilerName;
    }).ormolu;

    ip = unBreak (dontCheck super.ip);
    wide-word = unBreak (dontCheck (doJailbreak super.wide-word));

    relude =
      if super ? relude_0_6_0_0
      then super.relude_0_6_0_0
      else super.relude;
  };
}
