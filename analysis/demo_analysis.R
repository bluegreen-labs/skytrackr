# load libraries and functions
library(dplyr)
library(skytrackr)

files <- list.files(
  "~/Dropbox/Research_Projects/code_repository/bitbucket/apus_lunar_synchrony/data-raw/geolocators/Gent_Voorhaven_A_apus/",
  glob2rx("*.lux"),
  full.names = TRUE
)

files <- files[!grepl("drift", files)]
data <- lapply(files, function(file){
  stk_read_lux(file)
})

data <- do.call("rbind", data) |>
  dplyr::filter(
    logger == "CC874"
  )

options("dplyr.show_progress" = FALSE)
# batch processing via pipe
locations <- data |>
  group_by(logger) |>
  do({
    skytrackr(
      .,
      bbox = c(-20, -40, 60, 55),
      plot = TRUE
    )
  })
