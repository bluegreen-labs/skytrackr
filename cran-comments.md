Dear CRAN team,

This is an update to the {skytrackr} package (given the changes, see below, I will increase the major version number to 2.0). The packages estimates the position of animals using geolocation by light through inverse model optimization and parameter estimation.

Major changes:

The package is now extended with three helper functions:
- stk_filter()
- stk_calibrate()
- stk_center()

Here, stk_filter() provides a way to consistently filter the data with the aim to only select twilight periods. The previous threshold based method in the main skytrackr() function could accidentally include daytime values if they were sufficiently low. The routine was also hidden in the main function and the selection could not be visualized. This is addressed by exposing the data selection routine, as well as including a plotting option for quick data inspection. The stk_center() function centers diurnal light profiles on midday. This is a helper function which allows for the correct calculation of twilight ranges when the data crosses the date line.

During optimization the "free" scale parameter needs to be set with care, as it competes with the estimated latitude (as both influence the steepness of the model results). Setting a realistic range to the scale parameter limits the chance of spurious latitude estimates (especially for noisy data). The stk_calibrate() function now estimates this scale range (light attenuation) across a whole dataset relative to an ideal maximum global irratidation value. Within the context of ornithology the vignettes now include a section on how perching birds (in vegetation) might cause light attenuation and how vegetation density relates to the scale parameter.

I've also included a new light model. The original diurnal model approach fits a light model for a single location (latitude, longitude) to all observations in a reference set (defined by a date and time). This implicitly assumes stationarity across all these observations. However, during migration fast movements (east-west) can influence light responses. The "individual" model calculates individual positions for each observation based on the assumption of following a rhumb line (or a course of constant bearing, i.e. loxodrome) between the previous location and the proposed value at the end of the current observation period. This approach comes at a speed penalty but is theoretically more correct, but its robustness is still being evaluated across various data sources.

Minor changes:

This update also traps a plot rendering errors during optimization in skytrackr(), to avoid stalling the processing.

The step-selection function defaults to NULL, which removes any influence of distance on the likelihood of accepting a proposed step. However, documentation still suggest using a step-selection function.

I've also updated the visualization routine to include a shaded relief background, rivers and lakes to aid in orientation relative to large geographic features.

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
