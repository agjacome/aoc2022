#!/usr/bin/env nix-shell
#!nix-shell -i bash

set -euo pipefail

declare -r fat_jar="out/aoc2022/assembly.dest/out.jar"

mill aoc2022.assembly

for input_file in input/*; do
    day=$(basename $input_file .txt)

    echo "=== DAY $day ==="
    time $fat_jar $day $input_file
    echo "=============="
    echo
done
