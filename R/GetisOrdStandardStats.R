#' Standard Getis-Ord G* statistics.
#' @author Viliam Simko
#'
#' @param r Input raster layer
#'
#' @return Returns a list used in the function \link{GetisOrd}:
#'   \item{rmean}{Global mean of all raster cells.}
#'   \item{n}{Number of all cells within the raster.}
#'   \item{SD}{Standard deviation of all raster cells (single value).}
#'   \item{MEAN}{Same as rmean in standard Getis-Ord.}
#'
#' @examples
#' library(raster)
#' r <- raster( matrix(rnorm(400), 20, 20) )
#' w <- weight_matrix_circular_fade(7, 2)
#' st <- GetisOrdStandardStats(r)
#' plot(GetisOrd(r, w, st))
#'
#' @export
GetisOrdStandardStats <- function(r) {
  # sanity checks
  stopifnot(is(r, "RasterLayer"))

  n <- sum(values(!is.na(r))) # because we use na.rm = TRUE later

  rmean <- mean(values(r), na.rm = TRUE)

  SD <- sqrt( mean(values(r^2), na.rm = TRUE) - rmean^2 )

  list(
    rmean = rmean,
    n = n,
    SD = SD,
    MEAN = rmean
  )
}
