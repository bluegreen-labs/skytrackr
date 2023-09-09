# skytrackr

[![R-CMD-check](https://github.com/bluegreen-labs/skytrackr/workflows/R-CMD-check/badge.svg)](https://github.com/bluegreen-labs/skytrackr/actions)
[![codecov](https://codecov.io/gh/bluegreen-labs/skytrackr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/bluegreen-labs/skytrackr)
![](https://cranlogs.r-pkg.org/badges/grand-total/skytrackr) 
![](https://www.r-pkg.org/badges/version/skytrackr)

A tool to calculate geolocation by light positions using sky illuminance values (in lux). The
 routine uses model optimization and parameter estimation to determine locations robustly.

## How to cite this package

You can cite this package like this "we obtained location estimates using the 
{skytrackr} R package (Hufkens 2023)". Here is the full
bibliographic reference to include in your reference list (don't forget
to update the 'last accessed' date):

> Hufkens, K. (2023). skytrackr: a sky illuminance location tracker. Zenodo. <https://doi.org/10.5281/zenodo.xxxx>.

## Installation

### development release

To install the development releases of the package run the following
commands:

``` r
if(!require(remotes)){install.packages("remotes")}
remotes::install_github("bluegreen-labs/skytrackr")
library("skytrackr")
```

Vignettes are not rendered by default, if you want to include additional
documentation please use:

``` r
if(!require(remotes)){install.packages("remotes")}
remotes::install_github("bluegreen-labs/skytrackr", build_vignettes = TRUE)
library("skytrackr")
```

## Use


