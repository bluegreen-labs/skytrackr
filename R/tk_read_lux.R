tk_read_lux <- function(file) {
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
    ) |>
    dplyr::select(
      date, date_time, time, lux
    )
}
