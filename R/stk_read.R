
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
