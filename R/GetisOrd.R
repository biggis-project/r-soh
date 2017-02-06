#' Getis-Ord G* statistics using a weight matrix.
#' Data on the border are padded with the mean value during convolution.
#'
#' @param r Input raster
#' @param w Square weight matrix with odd number of rows/columns.
#' @param st Expecting a list of local statistics, see
#'   \link{GetisOrdStandardStats} and \link{GetisOrdLocalStats}
#' @return New raster of the same size as the original input raster \code{r}.
#'
#' @export
GetisOrd <- function(r, w, st = GetisOrdStandardStats(r)) {
  # sanity checks
  stopifnot(is(r, "RasterLayer"))
  stopifnot(is.matrix(w))
  stopifnot(is.numeric(w))
  stopifnot(is.list(st))
  stopifnot(all(dim(st$focus) >= dim(w))) # focus too small

  rc <- focal(r, w = w, pad = TRUE,
              padValue = st$rmean, na.rm = TRUE, fun = sum)

  wsum <- sum(w)

  # TODO: corners still produce NAs
  d <- (st$n * sum(w*w) - wsum*wsum)
  d[d <= 0] <- NA

  Q <- sqrt( d / (st$n - 1) )

  (rc - st$MEAN * wsum) / (st$SD * Q)
}
