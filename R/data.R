#' Migrate Technology Ltd demo data
#'
#' Demo data for a single day of Common swift light logger
#' data as read from a Migrate Technology Ltd .lux file
#' using stk_read_lux().
#'
#' The format is consistent with what is required by
#' the skytrackr() routine.
#'
#' @format DataFrame
#' \describe{
#'   \item{logger}{logger ID}
#'   \item{date}{date}
#'   \item{date_time}{date and time}
#'   \item{hour}{decimal hour}
#'   \item{lux}{light levels in lux}
#' }
"cc876"

#' Land area polygon
#'
#' Vector polygon of world land areas to constrain
#' model optimization.
#'
#' @format sf
#' \describe{
#'   \item{MULTIPOLYGON}{sf multipolygon}
#' }
"land"
