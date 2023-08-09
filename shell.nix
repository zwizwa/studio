with import <nixpkgs> {};
mkShell {
  buildInputs = [
    gcc jack2
    # keep this line if you use bash
    bashInteractive
  ];
}
