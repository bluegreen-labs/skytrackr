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

#' AVONET trait list
#'
#' Limited copy of the AVONET trait database used
#' to calculate flight dynamic characteristics.
#'
#' @format DataFrame
#' \describe{
#'   \item{species}{species name}
#'   \item{order}{taxonomic order}
#'   \item{wing_length}{wing length (in mm)}
#'   \item{secondary}{length of the secondary (in mm)}
#'   \item{mass}{body mass (in gr)}
#' }
"avonet"
