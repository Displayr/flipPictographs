{ pkgs ? import <nixpkgs> {}, displayrUtils }:

pkgs.rPackages.buildRPackage {
  name = "flipPictographs";
  version = displayrUtils.extractRVersion (builtins.readFile ./DESCRIPTION); 
  src = ./.;
  description = ''Wrappers for the rhtmlPictographs package, using the flip project conventions.'';
  propagatedBuildInputs = with pkgs.rPackages; [ 
    flipStatistics
    bmp
    rhtmlPictographs
    flipTables
    flipTransformations
    png
    jpeg
    janitor
    verbs
    httr
    flipChartBasics
    flipU
    plotly
  ];
}
