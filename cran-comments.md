Dear CRAN team,

This is an update to the {skytrackr} package. The packages estimates the position of animals using geolocation by light through inverse model optimization and parameter estimation.

The package is now extended with two functions:
- stk_filter()
- stk_calibrate()

Here, stk_filter() provides a way to consistently filter the data with the aim to only select twilight periods. The previous threshold based method in the main skytrackr() function could accidentally include daytime values if they were sufficiently low. The routine was also hidden in the main function and could not be visualized. This is addressed by exposing the data selection routine, as well as including a plotting option for quick data inspection. The latter complements the stk_profile() function.

During optimization the "free" scale parameter needs to be set with care, as it competes with the estimated latitude (as both in part influence the steepness of the model results). Setting a realistic range to the scale parameter limits the chance of spurious latitude estimates (especially for noisy data). The stk_calibrate() function now estimates this scale range.

Other changes include trapping plot rendering errors during optimization in skytrackr(), to avoid stalling the processing. The addition of a moving window approach (fitting to multiple days at a time) can address noise issues, at the expense of spatial resolution.

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
