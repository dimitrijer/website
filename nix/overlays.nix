self: super: {
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
