#' Locality-aware Getis-Ord G* statistics.
#'
#' @param r input raster
#' @param focus Square matrix with odd number of rows/column and boolean cells.
#'
#' @return Returns a list containing:
#' \item{rmean}{Global mean of all raster cells.}
#' \item{n}{Number of all cells within the raster.}
#' \item{SD}{A raster where each cell corresponds to a focal standard deviation
#' from area around the cell given by the matrix \code{focus}}
#' \item{MEAN}{A raster where each cell corresponds to a focal mean from area
#' around the cell given by the matrix \code{focus}}
#' \item{focus}{Copy of the \code{focus} matrix given as input parameter.}
#'
#' @export
GetisOrdLocalStats <- function(r, focus_matrix) {
  # sanity checks
  stopifnot(is(r, "RasterLayer"))
  stopifnot(is.matrix(focus_matrix))
  stopifnot(is.logical(focus_matrix))

  rmean <- mean(values(r), na.rm = TRUE)

  n <- focal(r, w = focus_matrix,
             fun = function(m) sum(!is.na(m)),
             pad = TRUE)

  stopifnot( all(values(n > 1)) ) # sanity check

  # NOTE: be careful, becasue the focal function does not know about
  # the structure of our filter, so it uses ncell() function instead of n
  # Therefore, we need to write our own formulas for mean and stdev.

  MEAN <- focal(r, w = focus_matrix,
                # DO NOT USE: `fun = mean, na.rm = TRUE`
                fun = function(m) mean(m, na.rm = TRUE),
                pad = TRUE)

  r2mean <- focal(r^2, w = focus_matrix,
                  # DO NOT USE: `fun = mean, na.rm = TRUE`
                  fun = function(m) mean(m, na.rm = TRUE),
                  pad = TRUE)

  SD <- sqrt(r2mean - MEAN^2)

  list(
    rmean = rmean,
    n = n,
    MEAN = MEAN,
    SD = SD,
    focus = focus_matrix # just for debugging
  )
}
