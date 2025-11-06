
#' Filter twilight values by range
#'
#' Filter out twilight values by range, and
#' returns the data frame with a twilight (TRUE/FALSE)
#' column or the data frame with only twilight values
#' selected (filtered out).
#'
#' Generally used for internal process, but can be
#' useful for visualizations of profiles as well.
#'
#' @param data a skytrackr compatible data frame
#' @param range a range c(min, max) of valid values in lux
#' @param smooth smooth the data using a hampel filter with a window size
#'  of 3, and a multiplier of the MAD of 3. Original values are substituted,
#'  the values replaced are flagged in an `outlier` column in the returned
#'  data frame (default = TRUE)
#' @param plot plot daily profiles with the range filter applied
#' @param filter if TRUE only twilight values are returned if
#'  FALSE the data frame is returned with an annotation column
#'  called 'twilight' for further processing.
#' @param verbose Give detailed feedback (TRUE or FALSE, default = TRUE)
#'
#' @returns a skytrackr compatible data frame, either filtered
#'  to only include twilight values selected by the range parameter
#'  or with an additional 'twilight' column to annotate these values.
#' @export
#' @examples
#'
#' # filter values using the preset range, only annotate
#' df <- cc876 |> stk_filter(range = c(1.5, 400))
#'
#' # filter values using the preset range, only retain filtered values
#' df <- cc876 |> stk_filter(range = c(1.5, 400), filter = TRUE)

stk_filter <- function(
    data,
    range,
    smooth = TRUE,
    plot = FALSE,
    filter = FALSE,
    verbose = TRUE
){

  # unravel the light data
  data <- data |>
    dplyr::filter(
      .data$measurement == "lux"
    )

  # range check
  if (range[1] < min(data$value, na.rm = TRUE)){
    range[1] <- min(data$value, na.rm = TRUE)
    cli::cli_alert("Minimum range value out of range, set to {range[1]}")
  }

  # Hampel value with a window of 3
  if (smooth){
    if(verbose){
      cli::cli_alert(c(
        "Smoothing the data using a Hampel filter",
        "i" = "
           [outliers will be replaced with interpolated values]
         "
      )
      )
    }

    # calculate metrics for the Hampel filter
    window <- 3
    median <- zoo::rollmedian(data$value, window, fill = NA)
    mad <- zoo::rollapply(
      data$value,
      window,
      mad,
      align = "center",
      fill = NA,
      na.rm = TRUE
    )

    # calculate outliers as 3 * MAD
    data$outlier <- ifelse(abs(data$value - median) > 3 * mad, TRUE, FALSE)

    # substitute original values with median
    data <- data |>
      dplyr::mutate(
        value = ifelse(.data$outlier, median, .data$value)
      ) |>
      stats::na.omit()
  }

  # filter data
  data <- data |>
    dplyr::group_by(.data$logger, .data$date) |>
    dplyr::do({

      # select values in date range
      selected <- ifelse(
        .data$value > range[1] & .data$value < range[2],
        TRUE,
        FALSE
      )

      # find run lengths
      run_info <- rle(selected)

      # sort the longest two sections
      gr_len <- sort(
        run_info$length[run_info$value],
        decreasing = TRUE)[1:2]

      # create segment list
      seg_list <- list()

      # for every run length recreate the vector
      # with labels TRUE or FALSE based on the length
      # of the run
      for (i in seq_along(run_info$values)) {
        run_length <- run_info$lengths[i]
        if (run_length %in% gr_len) {
          segment_value <- TRUE
        } else {
          segment_value <- FALSE
        }
        seg_list[[i]] <- rep(segment_value, run_length)
      }

      seg_list <- unlist(seg_list)
      df <- .data
      df$selected <- seg_list
      df
    })

  # plot
  if(plot){
    p <- data |>
      ggplot2::ggplot() +
      ggplot2::geom_point(
        ggplot2::aes(
          .data$hour,
          log(.data$value),
          colour = .data$selected
        ),
        na.rm = TRUE
      ) +
      ggplot2::labs(
        x = "hour",
        y = "log(lux)",
        title = "Diurnal light profile"
      ) +
      ggplot2::scale_color_manual(
        values = c("black","red")
      ) +
      ggplot2::theme_bw() +
      ggplot2::facet_wrap(~.data$logger)
    plot(p)
  }

  # only retain twilight values
  if (filter){
    data <- data |>
      dplyr::filter(.data$selected)
  }

  return(data)
}
