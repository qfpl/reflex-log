{ reflex-platform ? import ./nix/reflex-platform.nix } :
let

  haskellPackages = reflex-platform.ghc.override {
    overrides = self: super: {
      ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
      ghcWithPackages = self.ghc.withPackages;
    };
  };

  reflex-log = haskellPackages.callPackage ./reflex-log.nix {};
in
  reflex-log
