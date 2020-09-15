#!/usr/bin/env bash

APPs=(
    com.jetbrains.intellij
    com.jetbrains.AppCode
    com.jetbrains.goland
    com.jetbrains.CLion
    com.jetbrains.PhpStorm
)

for app in "${APPs[@]}"; do
    echo "$app enable aatext"
    defaults write $app CGFontRenderingFontSmoothingDisabled -bool NO
done
