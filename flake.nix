{
  description = "streamly-filepath";

  inputs = {
    basepkgs.url = "git+ssh://git@github.com/composewell/streamly-packages?rev=71f2b6792a9f64be2c4bca5050cb1e08edd625b2";
    nixpkgs.follows = "basepkgs/nixpkgs";
    nixpkgs-darwin.follows = "basepkgs/nixpkgs-darwin";
  };

  outputs = { self, nixpkgs, nixpkgs-darwin, basepkgs }:
    basepkgs.nixpack.mkOutputs {
      inherit nixpkgs nixpkgs-darwin basepkgs;
      name = "streamly-filepath";
      sources = basepkgs.nixpack.lib.localSource "streamly-filepath" ./.;
      packages = basepkgs.nixpack.lib.devPackage "streamly-filepath";
      #sources = import ./sources.nix;
      #packages = import ./packages.nix;
    };
}
