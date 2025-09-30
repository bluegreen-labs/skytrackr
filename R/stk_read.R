
#' Read Migrate Technology .lux files
#'
#' Read Migrate Technology Ltd. `.lux` files and re-formats
#' them to a skytrackr compatible format.
#'
#' @param files A `.lux` file or list of `.lux` files with light level values
#' @param verbose provide detailed feedback
#'
#' @importFrom rlang .data
#' @return A skytrackr compatible data frame for use in further location
#'  estimation.
#' @export
#' @examples
#' # read in the demo lux file
#' df <- stk_read_lux(
#'  system.file("extdata/cc876.lux", package="skytrackr")
#' )

stk_read_lux <- function(files, verbose = TRUE) {

  if (verbose){
    cli::cli_div(
      theme = list(
        rule = list(
          color = "darkgrey",
          "line-type" = "double",
          "margin-bottom" = 1
        ),
        span.strong = list(color = "black"))
    )
    cli::cli_rule(
      left = "{.strong Reading data}",
      right = "{.pkg skytrackr v{packageVersion('skytrackr')}}"
    )
    cli::cli_end()
  }

  df_final <- lapply(files, function(file){

    if (verbose){
      cli::cli_alert_info(
        "Processing file: {.strong {basename(file)} }!")
    }

    # check if there matching logger activity
    # files to merge with
    deg_file <- paste0(tools::file_path_sans_ext(file),".deg")

    if (file.exists(deg_file)) {
      deg <- read_deg_lux(deg_file)

      if (verbose){
        cli::cli_alert_info(
          "Ancillary data found and read!")
      }
    }

    # read lux file by default
    df <- read_deg_lux(file, verbose = verbose)

    # merge with ancillary values if available
    if (file.exists(deg_file)) {
      df <- dplyr::bind_rows(df, deg)
    }

    return(df)
  })

  # bind rows of list elements
  df_final <- dplyr::bind_rows(df_final)

  # return data
  return(df_final)
}

#' Read Swiss Ornithology institute GLF files
#'
#' Read Swiss Ornithology institute files in the `.glf` and re-formats
#' them to a skytrackr compatible format.
#'
#' @param files A `.glf` file or list of `.glf` files with light level values.
#' @param verbose provide detailed feedback
#'
#' @return A skytrackr compatible data frame for use in further location
#'  estimation.
#' @export
#' @examples
#' \dontrun{
#' df <- stk_read_glf("your_SOI_glf_file.glf")
#' }

stk_read_glf <- function(files, verbose = TRUE) {

  df_final <- lapply(files, function(file){

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

    if (verbose){
      header <- readLines(file, n = 6)
      cli::cli_h2("Header information")
      cli::cli_blockquote(
        paste(header, collapse = "\n")
      )
    }

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
  })

  # bind rows of list elements
  df_final <- dplyr::bind_rows(df_final)

  # return data
  return(df_final)
}

#' Read lux and deg files
#'
#' This function is wrapped by the `stk_read_lux()` function.
#'
#' @param file A lux or deg file.
#' @param verbose provide detailed feedback
#'
#' @return A skytrackr data frame with logger data.
#' @export
#' @importFrom rlang .data

read_deg_lux <- function(file, verbose = TRUE) {

  # read in logger number
  logger <- readLines(file, n = 3)[3]
  logger <- strsplit(logger, ": ")[[1]][2]

  df <- utils::read.table(
    file,
    header = TRUE,
    skip = 19
  )

  if (verbose){
    header <- readLines(file, n = 19)
    cli::cli_h2("Header information")
    cli::cli_blockquote(
      paste(header, collapse = "\n")
    )
  }

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
