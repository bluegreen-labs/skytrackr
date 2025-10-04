Dear Konstanze, CRAN team,

This is a re-submission of the {skytrackr} package. The packages estimates the position of animals using geolocation by light through inverse model optimization and parameter estimation.

The updates include small issues found: including a \dontrun statement (replaced with \donttest as too calculation/time intensive, removal of a stray set.seed() reference in the stk_cluster() function and the inclusion of a doi reference to underlying theory (although not fully applicable as the applied optimization is calibration free). 

In addition, through testing with collaborators some other improvements were made obvious. In particular, the data ingestion now supports batch processing (both reading and screening the data for outliers). The feedback of these routines has also improved. Plotting data has improved to allow for uneven time steps (which happens in some instances).

Additional documentation on parallization using {multidplyr} and general advice on optimization strategies is now provided in two additional vignettes.

Kind regards,
Koen Hufkens

--- 

I have read and agree to the the CRAN policies at:
http://cran.r-project.org/web/packages/policies.html

## local, github actions and r-hub

- Ubuntu 22.04 install on R 4.3.1 (local)
- Ubuntu 22.04 on github actions (devel / release)
- codecove.io code coverage at ~91%

## github actions and local R CMD check results

0 errors | 0 warnings | 0 notes
