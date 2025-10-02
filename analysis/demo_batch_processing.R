library(skytrackr)
library(dplyr)
library(multidplyr)

# creating a fake dataset
# by duplicating data and
# renaming the logger
df1 <- skytrackr::cc876
df1$logger <- "CC888"
df2 <- skytrackr::cc876
df <- bind_rows(df1,df2)

# create a new cluster
# where the "skytrackr" library
# is made available
cluster <- new_cluster(2)
cluster_library(cluster, "skytrackr")

# split tasks by logger
# across cluster partitions
df_logger <- df |>
  group_by(logger) |>
  partition(cluster)

# run the analysis in parallel
# on the cluster (local or remote)
locations <- df_logger |>
  group_by(logger) |>
  do({

    # set seed per parallel unit
    set.seed(1)

    # define land mask
    mask <- stk_mask(
      bbox  =  c(-20, -40, 60, 60),
      buffer = 150, # in km
      resolution = 0.5 # in degrees
    )

    # define land mask with a bounding box
    # and an off-shore buffer (in km), in addition
    # you can specifiy the resolution of the resulting raster
    mask <- stk_mask(
      bbox  =  c(-20, -40, 60, 60), #xmin, ymin, xmax, ymax
      buffer = 150, # in km
      resolution = 0.5 # map grid in degrees
    )

    # define a step selection distribution
    ssf <- function(x, shape = 0.9, scale = 100, tolerance = 1500){
      # normalize over expected range with km increments
      norm <- sum(stats::dgamma(1:tolerance, shape = shape, scale = scale))
      prob <- stats::dgamma(x, shape = shape, scale = scale) / norm
      return(prob)
    }

    skytrackr(
      .data,
      mask = mask,
      plot = FALSE,
      verbose = FALSE,
      start_location = c(51.08, 3.73), # Gent - lux file
      tolerance = 1500, # in km
      scale = log(c(0.00001, 50)),
      range = c(0.09, 148),
      control = list(
        sampler = 'DEzs',
        settings = list(
          burnin = 250,
          iterations = 300,
          message = FALSE
        )
      ),
      step_selection = ssf
    )
  })

# drop the parallel processing info
# locations <- locations |>
#   as.data.frame()
