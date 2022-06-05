self: super: {
  # Make ghc8107 the default package set for Haskell.
  haskellPackages = super.haskell.packages.ghc902.override {
    overrides = self: super: { };
  };
  haskell-language-server = super.haskell-language-server.override {
    supportedGhcVersions = [ "902" ];
  };
  # Override ghc with specific version. Add dependencies so they show up in the
  # shell -- they don't need to explicitly be specified for the site, as deps
  # are explicitly specified in the derivation.
  ghc = self.haskellPackages.ghcWithPackages (
    ps: with ps; [
      pandoc
      hakyll
      time
      text
      process
      skylighting
      skylighting-core
      containers
    ]
  );
}
