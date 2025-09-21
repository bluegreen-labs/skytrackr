
#' Read Migrate Technology .lux files
#'
#' Read Migrate Technology lux files and re-formats
#' them to a skytrackr compatible format.
#'
#' @param file a .lux file with light level values
#'
#' @importFrom rlang .data
#' @return a skytrackr compatible data frame for use in further location
#'  estimation
#' @export

stk_read_lux <- function(file) {

  # check if there matching logger activity
  # files to merge with
  deg_file <- paste0(tools::file_path_sans_ext(file),".deg")

  if (file.exists(deg_file)) {
    deg <- read_deg_lux(deg_file)
  }

  # read lux file by default
  df <- read_deg_lux(file)

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
    col.names = c("date_time","lux","1","2","3")
  )[,1:2]

  df <- df |>
    dplyr::mutate(
      logger = logger,
      date_time = as.POSIXct(
        .data$date_time,
      "%d.%m.%Y %H:%M",
      tz = "GMT"
      ),
      date = as.Date(.data$date_time, "%d/%m/%Y"),
      hour = as.numeric(format(.data$date_time,"%H")) +
        as.numeric(format(.data$date_time,"%M"))/60 +
        as.numeric(format(.data$date_time,"%S"))/3600,
      lux = as.double(.data$lux)
    )

  df <- df |>
    tidyr::pivot_longer(
      cols = tidyr::any_of("lux"),
      values_to = "value",
      names_to = "measurement"
    )

  return(df)
}

#' Read lux and deg files
#'
#' @param file a lux or deg file
#'
#' @return a data frame with logger data
#' @export
#' @importFrom rlang .data

read_deg_lux <- function(file) {

  # read in logger number
  logger <- readLines(file, n = 3)[3]
  logger <- strsplit(logger, ": ")[[1]][2]

  df <- utils::read.table(
    file,
    header = TRUE,
    skip = 19
  )

  df <- df |>
    dplyr::mutate(
      logger = logger,
      date_time = as.POSIXct(
        paste(.data$'DD.MM.YYYY', .data$'HH.MM.SS'),
        "%d/%m/%Y %H:%M:%S",
        tz = "GMT"
      ),
      date = as.Date(.data$date_time, "%d/%m/%Y"),
      hour = as.numeric(format(.data$date_time,"%H")) +
        as.numeric(format(.data$date_time,"%M"))/60 +
        as.numeric(format(.data$date_time,"%S"))/3600
    ) |>
    dplyr::select(
      -"DD.MM.YYYY",
      -"HH.MM.SS"
    )

  df <- df |>
    tidyr::pivot_longer(
      cols = tidyr::any_of(c("light.lux.", "T..C.","P.Pa.","Xavrg","Zact")),
      values_to = "value",
      names_to = "measurement"
    )


  # harmonize measurement names
  df <- df |>
    dplyr::mutate(
      measurement = dplyr::recode(
        .data$measurement,
        "light.lux." = "lux",
        "T..C." = "temperature",
        "P.Pa." = "pressure",
        "Xavrg" = "X",
        "Zact" = "Z"
      )
    )
}
