
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

  # check if there matching logger activity
  # files to merge with
  deg_file <- paste0(tools::file_path_sans_ext(file),".deg")

  if (file.exists(deg_file)) {
    deg <- utils::read.table(
      deg_file,
      header = TRUE,
      skip = 19
    )

    deg$date_time <- as.POSIXct(paste(
      deg$'DD.MM.YYYY', deg$'HH.MM.SS'
    ),
    "%d/%m/%Y %H:%M:%S",
    tz = "GMT"
    )

    deg$date = as.Date(deg$date, "%d/%m/%Y")
    deg$hour <- as.numeric(format(deg$date_time,"%H")) +
      as.numeric(format(deg$date_time,"%M"))/60 +
      as.numeric(format(deg$date_time,"%S"))/3600

    deg <- deg |>
      dplyr::select(
        -"DD.MM.YYYY",
        -"HH.MM.SS"
      ) |>
      tidyr::pivot_longer(
        cols = any_of(c("T..C.","P.Pa.","Xavrg","Zact")),
        values_to = "value",
        names_to = "measurement"
      )
  }

  # read in logger number
  logger <- readLines(file, n = 3)[3]
  logger <- strsplit(logger, ": ")[[1]][2]

  # read in Migrate Tech lux fi
  # into the correct format
  df <- utils::read.table(
    file,
    header = TRUE,
    skip = 19,
    col.names = c("date","time","lux")
  )

  df$date_time <- as.POSIXct(paste(
    df$date, df$time
  ),
  "%d/%m/%Y %H:%M:%S",
  tz = "GMT"
  )

  df$date = as.Date(df$date, "%d/%m/%Y")

  df$logger <- logger
  df$hour <- as.numeric(format(df$date_time,"%H")) +
        as.numeric(format(df$date_time,"%M"))/60 +
        as.numeric(format(df$date_time,"%S"))/3600

  df <- df[,c("logger", "date", "date_time", "hour", "lux")]

  df <- df |>
    tidyr::pivot_longer(
    cols = "lux",
    values_to = "value",
    names_to = "measurement"
  )

  # merge with ancillary values if available
  if (file.exists(deg_file)) {
    df <- dplyr::bind_rows(df, deg)
  }

  return(df)
}

#' Read GLF files
#'
#' Read Swiss Ornithology institute files
#'
#' @param file a .glf file with light level values
#'
#' @return a skytrackr compatible data frame for use in further location
#'  estimation
#' @export

stk_read_glf <- function(file) {

  # read in logger number
  logger <- readLines(file, n = 1)[1]
  logger <- strsplit(logger, ": ")[[1]][2]

  # read GLF format
  df <- utils::read.table(
    file,
    header = TRUE,
    sep = "\t",
    skip = 6,
    col.names = c("date_time","light","1","2","3")
  )

  df$date_time <- as.POSIXct(paste(
    df$date, df$time
  ),
  "%d/%m/%Y %H:%M:%S",
  tz = "GMT"
  )

  df$date = as.Date(df$date_time, "%d/%m/%Y")

  df$logger <- logger
  df$hour <- as.numeric(format(df$date_time,"%H")) +
    as.numeric(format(df$date_time,"%M"))/60 +
    as.numeric(format(df$date_time,"%S"))/3600

  df <- df[,c("logger", "date", "date_time", "hour", "lux")]

  return(df)
}
