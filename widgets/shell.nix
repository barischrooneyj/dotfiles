{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {

  buildInputs = [
    pkgs.cairo
    pkgs.glib
    pkgs.gtk3
    pkgs.gobject-introspection
    pkgs.pkgconfig
    pkgs.python3
    pkgs.poetry
  ];

}
