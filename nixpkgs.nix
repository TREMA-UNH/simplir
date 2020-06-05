let
  rev = "698e71db2c47b5d3a206c65288f88ab4320eaf1";
  sha256 = "1lwlxjb38mis83hfi6d4i8yrblb31zvk5ff5wx9csghbk2pki9w1";
  tarball = builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs-channels/archive/${rev}.tar.gz";
    inherit sha256;
  };
in import tarball
