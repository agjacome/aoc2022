let pkgs = import <nixpkgs> {};

in pkgs.mkShell rec {
  name = "aoc2022";

  buildInputs = with pkgs; [ git jdk mill scala ];
}
