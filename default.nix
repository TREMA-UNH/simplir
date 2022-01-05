{ nixpkgs ? (import ./nixpkgs.nix {}) }:

let
  inherit (nixpkgs.haskell.lib) dontCheck doJailbreak
                                enableDWARFDebugging enableExecutableProfiling;
  inherit (nixpkgs.stdenv) lib;
  inherit (nixpkgs) fetchFromGitHub;

  localDir = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  trec-eval = nixpkgs.callPackage ./trec-eval.nix {};

  all-cabal-hashes =
    let
      rev = "30a0c2f2c25056349249cda6aec4428c2229e3b8";
    in builtins.fetchurl {
      url    = "https://github.com/commercialhaskell/all-cabal-hashes/archive/${rev}.tar.gz";
      sha256 = "1a3zvq1yr4wm335y8zndn08d3yjjg51kk6p8lx11jpn1j28si0k8";
    };

  haskellOverrides = self: super:
    let
      otherOverrides = {
        mkDerivation         = args: super.mkDerivation (args // {
          dontStrip = true;
          configureFlags =
            #["--profiling-detail=toplevel-functions"] ++
            (args.configureFlags or []) ++
            [ "--ghc-options=-g3"
              "--disable-executable-stripping"
              "--disable-library-stripping"
              #"--ghc-options=-eventlog"
            ];
        });
      };

      simplirPackages = {
        simplir              = self.callCabal2nix "simplir" (localDir ./simplir) {};
        simplir-data-source  = self.callCabal2nix "simplir-data-source" (localDir ./simplir-data-source) {};
        simplir-html-clean   = self.callCabal2nix "simplir-html-clean" (localDir ./simplir-html-clean) {};
        simplir-trec         = self.callCabal2nix "simplir-trec" (localDir ./simplir-trec) {};
        simplir-galago       = self.callCabal2nix "simplir-galago" (localDir ./simplir-galago) {};
        #simplir-tools        = self.callCabal2nix "simplir-tools" (localDir ./simplir-tools) {};
        simplir-word-embedding = self.callCabal2nix "simplir-word-embedding" (localDir ./simplir-word-embedding) {};
        simplir-trec-streaming = self.callCabal2nix "simplir-trec-streaming" (localDir ./simplir-trec-streaming) {};
        simplir-kyoto-index  = self.callCabal2nix "simplir-kyoto-index" (localDir ./simplir-kyoto-index) {};
        simplir-leveldb-index = self.callCabal2nix "simplir-leveldb-index" (localDir ./simplir-leveldb-index) {};
        simplir-disk-index   = self.callCabal2nix "simplir-disk-index" (localDir ./simplir-disk-index) {};
        simplir-eval         = let base = self.callCabal2nix "simplir-eval" (localDir ./simplir-eval) {};
                               in nixpkgs.haskell.lib.overrideCabal base (drv: { testDepends = [ trec-eval ]; });
        simplir-stop-words   = self.callCabal2nix "simplir-stop-words" (localDir ./simplir-stop-words) {};
        simplir-learning-to-rank
                             = self.callCabal2nix "simplir-learning-to-rank" (localDir ./simplir-learning-to-rank) {};
        simplir-pipes-utils  = self.callCabal2nix "simplir-pipes-utils" (localDir ./simplir-pipes-utils) {};
        simplir-io           = self.callCabal2nix "simplir-io" (localDir ./simplir-io) {};
        http-parsers         = self.callCabal2nix "http-parsers" ./vendor/http-parsers {};
        indexed-vector       = self.callCabal2nix "indexed-vector" ./vendor/indexed-vector {};
        fork-map             = self.callCabal2nix "fork-map" ./vendor/fork-map {};

        lzma = dontCheck super.lzma;
        ListLike = doJailbreak super.ListLike;
        text-icu   = dontCheck super.text-icu;
        streaming-commons = dontCheck super.streaming-commons;
        pipes-zlib = doJailbreak super.pipes-zlib;
        pipes-text = doJailbreak (super.callHackage "pipes-text" "0.0.2.5" {});
        pipes-bzip = dontCheck (doJailbreak (super.callHackage "pipes-bzip" "0.2.0.4" { bzlib = null; }));
        pipes-lzma = doJailbreak (super.callHackage "pipes-lzma" "0.2.0.0" {});
        pipes-interleave = doJailbreak (super.callHackage "pipes-interleave" "1.1.3" {});
        html-parse = self.callCabal2nix "html-parse" ./vendor/html-parse {};
        b-tree = doJailbreak (self.callHackage "b-tree" "0.1.4" {});
        warc = self.callCabal2nix "warc" (nixpkgs.fetchFromGitLab {
          domain = "git.smart-cactus.org";
          owner = "ben";
          repo = "warc";
          rev = "3c052804e4f17bf76f2d24c565e7ab975b0ad7e1";
          sha256 = "sha256:027b3zjvrav7qbjgn40xkdacv62745y8n0krwkwdkhqs577rznfs";
        }) {};

        pinch = doJailbreak (dontCheck (self.callCabal2nix "pinch" (nixpkgs.fetchFromGitHub {
          owner = "abhinav";
          repo = "pinch";
          rev = "b8516191d7ed87231794471cee03f9c842e60578";
          sha256 = "sha256:0iy85wxava4pn2hnrcw0p4vgbwndx95ilirk05zi6p30z17dn0yr";
        }) {}));
        pipes-safe = self.callHackage "pipes-safe" "2.3.1" {};
      };
    in otherOverrides // simplirPackages // { simplirPackages = simplirPackages; };

  ghcVersion = "ghc8107";
  haskellPackages = nixpkgs.haskell.packages."${ghcVersion}".override {
    inherit all-cabal-hashes;
    overrides = haskellOverrides;
  };
in {
  inherit ghcVersion haskellPackages haskellOverrides;
  inherit trec-eval;
  inherit (haskellPackages) simplirPackages;
  env = haskellPackages.ghcWithHoogle (pkgs: builtins.attrValues haskellPackages.simplirPackages);
}
