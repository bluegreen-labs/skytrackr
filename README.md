# skytrackr

[![R-CMD-check](https://github.com/bluegreen-labs/skytrackr/workflows/R-CMD-check/badge.svg)](https://github.com/bluegreen-labs/skytrackr/actions)
[![codecov](https://codecov.io/gh/bluegreen-labs/skytrackr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/bluegreen-labs/skytrackr)
![](https://cranlogs.r-pkg.org/badges/grand-total/skytrackr) 
![](https://www.r-pkg.org/badges/version/skytrackr)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.8331492.svg)](https://doi.org/10.5281/zenodo.8331492)

A tool to calculate geolocation by light positions using sky illuminance values (in lux). The
 routine uses model optimization and parameter estimation to determine locations robustly.

## How to cite this package

You can cite this package like this "we obtained location estimates using the 
{skytrackr} R package (Hufkens 2023)". Here is the full
bibliographic reference to include in your reference list (don't forget
to update the 'last accessed' date):

> Hufkens, K. (2023). skytrackr: a sky illuminance location tracker. Zenodo. <https://doi.org/10.5281/zenodo.8331492>.

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

To demonstrate the functioning of the package a small demo dataset comprised of a single day of light logging of a Common swift near a nest box in Ghent, Belgium was included (i.e. tag cc874). I will use this data to demonstrate the features of the package. Note that when multiple dates are present all dates will be considered. The package is friendly to the use of R piped (|>) commands. Using all default settings you can just pipe the data in to the `skytrackr()` function. The returned object will be a data frame containing the best estimate (median) of the longitude and latitude as well as 5-95% quantile as sampled from the posterior parameter distribution.

## Default Sequential Monte Carlo optimization

The underlying BayesianTools package used in optimization allows for the specification of other optimization techniques including Sequential Monte Carlo (SMC) methods, which in effect are particle filters (considering multiple priors during optimization). This method is considerably faster than the default DEzs method while fostering better accuracy during equinox periods. This is the default, but care should be taken to specify the start location and tolerance (maximum degrees covered in a single flight).

```r
# normally you would first read in the .lux or .glf file using
# cc874 <- stk_read_lux("cc874.lux")

# load the integrated package data
print(head(skytrackr::cc874))

# skytrackr allows for piped data input
location <- cc874 |>
  skytrackr::skytrackr(
      start_location = c(51.08, 3.73),
      tolerance = 11,
  )
```

## MCMC DEzs optimization

Changing the control parameters allows for switching to a MCMC DEzs methodology.

```r
data |>
    skytrackr(
      start_location = c(51.08, 3.73),
      tolerance = 11,
      bbox = c(-20, -40, 60, 60),
      control = list(
          sampler = 'DEzs',
          settings = list(
              burnin = 2000,
              iterations = 10000,
              message = FALSE
          )
        ),
      plot = FALSE
    )
```

## Comparison

Comparison between the SMC (a) and DEzs method (b), with SMC being better constrained.

![](https://github.com/bluegreen-labs/skytrackr/blob/main/smc_dezs_comparison.png)
