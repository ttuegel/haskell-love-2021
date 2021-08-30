let sources = import ./nix/sources.nix; in

{
  pkgs ? import sources."nixpkgs" {}
}:

let
  inherit (pkgs) lib haskell haskellPackages;
  overridePlan = original: replace: drv:
    drv.overrideScope (self: super: { "${original}" = self."${replace}"; });
  patat =
    lib.foldr
      (f: x: f x)
      haskellPackages.patat
      [
        (overridePlan "optparse-applicative" "optparse-applicative_0_15_1_0")
        haskell.lib.doJailbreak
      ];
  ghc = haskellPackages.ghcWithPackages (hspkgs: with hspkgs; [
    random
  ]);
in

pkgs.mkShell {
  buildInputs = with pkgs; with haskellPackages; [
    ghc patat hp2pretty markdown-unlit
  ];
}
