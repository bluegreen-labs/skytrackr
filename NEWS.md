# skytrackr 1.1

Where release 1.0 was tuned for use on the common swift (an ideal light logger species)
this release sees improvements in which cover expanding the methodology to
species which have less than optimal (noisy) light logger data. It provides new
methods to correctly estimate parameter ranges (attenuation factors due to
preferred secluded habitats).

- introduction of a filter function `stk_filter()`, but helpful in visualizing data
- introduction of the `stk_calibrate()` function to estimate scale factors for optimization
- correction to the subsetting of the data (see `stk_filter()`) to accommodate noisy data
- trap rendering error on intermediate plots, which corrupts optimization

# skytrackr 1.0

- removed seed from `stk_cluster()`
- move \dontrun to \donttest wrappers
- added references to theory papers
- support batch reading of files
- support batch screening of data using `stk_screen_twl()`
- verbose feedback on data reading
- allow plotting of data with uneven data steps (in time)
- assign a placeholder "logger" name in the mapping function `stk_map()`
- vignette on parallel processing using {multidplyr}
- vignette on optimization improvement strategies
- adding reference into package DESCRIPTION file
- CRAN compliant release

# skytrackr 0.9

- draft release and working proof-of-concept

