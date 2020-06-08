{ sources ? import ./nix/sources.nix, pkgs ? import sources.nixpkgs { }
, nix-hs ? import sources.nix-hs { inherit pkgs; }, ghc ? "default" }:

nix-hs {
  cabal = ./addy.cabal;
  compiler = ghc;

  overrides = lib: self: super:
    with lib; {
      ip = unBreak (dontCheck super.ip);
      wide-word = unBreak (dontCheck (doJailbreak super.wide-word));
    };
}
