Dear CRAN team,

This is a new (first) submission of the {skytrackr} package. The packages estimates the position of animals using geolocation by light through inverse model optimization and parameter estimation. Locations estimates are therefore based on a template matching approach combined with behaviourly informed constraints (a step-selection function, i.e. constraints on how far an animal can move from the previous position to the next). The package leverages my {skylight} package (recently updated for speed for use within {skytrackr}) in order to model the diurnal light profile (based upon a physical model).

The approach differs from other packages such as {FlightR} which relies on a more limited subset of the light logger data (mostly the timing of sunrise and sunset, and the timing of noon). The optimization physical model based approach (or template matching) to geolocation by light is more robust,and requires less pre-processing than conventional methodologies - with the added advantage of acquiring uncertainty metrics during optimization. The package therefore provides as much of a hands-off methodology as possible, allowing for a streamlined workflow with limited time consuming user interaction for a maximum of information.

I hope this is valuable addition to the existing movement ecology packages.

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
