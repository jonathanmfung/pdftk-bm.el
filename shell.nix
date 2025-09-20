let
  pkgs =
    import
      # 8a3f0f0 is 250915
      (fetchTarball "https://github.com/NixOS/nixpkgs/archive/8a3f0f06a41ced113e31862e8a503b8895a098e1.tar.gz")
      { };
in
pkgs.mkShell {
  pname = "pdftk-bm";
  packages = [
    # dev
    pkgs.eask-cli

    # required
    pkgs.pdftk
  ];
}
