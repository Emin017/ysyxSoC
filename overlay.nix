final: prev: {
  mill = prev.mill.overrideAttrs (oldAttrs: rec {
    version = "0.12.1";
    src = prev.fetchurl {
      url = "https://github.com/com-lihaoyi/mill/releases/download/${version}/${version}-assembly";
      hash = "sha256-/Mr9aSeSW7DmwwsATU9U8/NysvSao/VWxUWlQ5w7dx8=";
    };
  });
  # circt = prev.circt.overrideAttrs (oldAttrs: rec {
  #   version = "1.66.0";
  #   src = prev.fetchFromGitHub {
  #       owner = "llvm";
  #       repo = "circt";
  #       rev = "firtool-${version}";
  #       hash = "sha256-pIuBIl1iZRuqjy7CPfsTnR82Fq7iH22TtpbSk4oBshQ=";
  #       fetchSubmodules = true;
  #   };
  # });
}

