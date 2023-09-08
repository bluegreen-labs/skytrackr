
#' Read Migrate Technology .lux files
#'
#' Read Migrate Technology lux files and re-formats
#' them to a skytrackr compatible format.
#'
#' @param file a .lux file with light level values
#'
#' @return a skytrackr compatible data frame for use in further location
#'  estimation
#' @export

stk_read_lux <- function(file) {

  # read in logger number
  logger <- readLines(file, n = 3)[3]
  logger <- strsplit(logger, ": ")[[1]][2]

  # read in Migrate Tech lux fi
  # into the correct format
  df <- read.table(
    file,
    header = TRUE,
    skip = 19,
    col.names = c("date","time","lux")
  ) |>
    dplyr::mutate(
      date_time = as.POSIXct(paste(
        date, time
      ),
      "%d/%m/%Y %H:%M:%S",
      tz = "GMT"
      ),
      date = as.Date(date, "%d/%m/%Y"),
      logger = logger
    ) |>
    dplyr::select(
      logger, date, date_time, time, lux
    )

  return(df)
}
