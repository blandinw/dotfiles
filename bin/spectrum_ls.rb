#!/usr/bin/env zsh

# Show all 256 colors with color number
for code in {000..255}; do
  print -P -- "$code: %F{$code}%n@%m%f"
done
