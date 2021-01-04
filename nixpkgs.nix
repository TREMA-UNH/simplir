let
  rev = "e065200fc90175a8f6e50e76ef10a48786126e1c";
  sha256 = "sha256:157ih9h1j9r4rf5ppv4yhin73k664bzclsy9c791crx9j5db0l7a";
  tarball = builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };
in import tarball
