
# files <- list.files(
#   "~/Dropbox/Research_Projects/code_repository/bitbucket/apus_lunar_synchrony/data-raw/geolocators/Gent_Voorhaven_A_apus/",
#   glob2rx("*.lux"),
#   full.names = TRUE
# )
#
# files <- files[grepl("drift", files)]
#
# data <- lapply(files, function(file){
#   stk_read_lux(file)
# })
#
# data <- do.call("rbind", data)
#
# data <- data |>
#   dplyr::filter(
#     logger == "CC874"
#   )
