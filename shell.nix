{ pkgs ? import <nixpkgs> {}, compiler ? "ghc822", doBenchmark ? false }:

let

f = { mkDerivation, haskell, base, foldl, Frames, fuzzyset
    , inline-r, integration, lens, libintlOrEmpty
    , pandoc-types, plots
    , diagrams-rasterific
    , diagrams
    , diagrams-svg
    , diagrams-contrib
    , hmatrix-gsl
    , R, random, random-source, random-fu, sundials, stdenv
    , template-haskell, temporary }:
mkDerivation {
  pname = "mrp";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base
    diagrams
    diagrams-rasterific
    diagrams-svg
    diagrams-contrib
    (haskell.lib.dontCheck inline-r)
    foldl
    Frames
    fuzzyset
    hmatrix-gsl
    integration
    lens
    pandoc-types
    plots
    random
    random-source
    random-fu
    template-haskell
    temporary
  ];
  buildDepends = [ libintlOrEmpty sundials];
  executableSystemDepends = [
    R
    pkgs.rPackages.bayesplot
    pkgs.rPackages.codetools
    pkgs.rPackages.dplyr
    pkgs.rPackages.ggmap
    pkgs.rPackages.ggplot2
    pkgs.rPackages.KernSmooth
    pkgs.rPackages.knitr
    pkgs.rPackages.maptools
    pkgs.rPackages.reshape2
    pkgs.rPackages.rgeos
    pkgs.rPackages.rgdal
    pkgs.rPackages.rstan
    pkgs.rPackages.zoo];
  license = stdenv.lib.licenses.bsd3;
};

haskellPackages = if compiler == "default"
  then pkgs.haskellPackages
  else pkgs.haskell.packages.${compiler};

# Fixup a few things
myHaskellPackages = haskellPackages.override {
  overrides = self: super: with pkgs.haskell.lib; {
    diagrams-rasterific = doJailbreak super.diagrams-rasterific;
    diagrams-svg = doJailbreak super.diagrams-svg;
    diagrams-contrib = doJailbreak super.diagrams-contrib;
    diagrams = doJailbreak super.diagrams;
    inline-r = dontCheck super.inline-r;
    pipes-group = doJailbreak super.pipes-group;
  };
};

variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

drv = variant (myHaskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
