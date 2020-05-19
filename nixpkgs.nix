let
  rev = "82b5f87fcc710a99c47c5ffe441589807a8202af";
  sha256 = "0wz07gzapdj95h9gf0rdc2ywgd7fnaivnf4vhwyh5gx24dblg7q8";
  tarball = builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs-channels/archive/${rev}.tar.gz";
    inherit sha256;
  };
in import tarball
