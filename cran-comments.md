Dear CRAN team,

This is a new (first) submission of the {skytrackr} package. The packages is a convenient tool to track the position of animals using geolocation by light. The package leverages my previous {skylight} package in order to model the diurnal light profile (based upon a physical model). Model results are optimized for a given registered (logged) light profile, where the optimal parameters (latitude, longitude, sky conditions) provide an estimated position.

The approach differs from other packages such as {FlightR} which relies on a more limited subset of the light logger data (mostly the timing of sunrise and sunset, and the timing of noon). Relying on a more limited subset of data is arguably faster, but more prone to small disturbances in the estimation of sunset / sunrise timing. My optimization physical model based approach to geolocation by light is more robust,and requires less pre-processing than conventional methodologies. 

I hope this is valuable addition to the existing movement ecology packages within this context.

Kind regards,
Koen Hufkens

--- 

I have read and agree to the the CRAN policies at:
http://cran.r-project.org/web/packages/policies.html

## local, github actions and r-hub

- Ubuntu 22.04 install on R 4.3.1 (local)
- Ubuntu 22.04 on github actions (devel / release)
- rhub::check_on_cran()
- codecove.io code coverage at ~57%

## github actions and local R CMD check results

0 errors | 0 warnings | 0 notes
