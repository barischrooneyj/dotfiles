#!/bin/sh

cp /home/jeremy/.config/nixpkgs/home.nix /home/jeremy/dotfiles/home.nix
cp /etc/nixos/configuration.nix          /home/jeremy/dotfiles/configuration.nix
git commit -a
git push
