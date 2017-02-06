# TODO: cleanup and split into multiple files
7
# Preprocessing ----------------------------------------------------------------

#' Helper function that can be used for picking the extend manually from
#' a plotted raster. (use mouse to pick coordinates on the map)
#' @param r raster
#' @return \link{Extent} object representing the selected area
select_extent_manually <- function(r) {
  plot(r)
  drawExtent()
}


# Plotting functions -----------------------------------------------------------

plot_color_table <- function(ct) {
  x <- raster(nrows = length(ct), ncols = 1, vals = 0:(length(ct) - 1))
  par(mar = c(0,1,0,1))
  plot(x, col = ct, axes = FALSE, box = FALSE)
}

interpolate_colortable <- function(howmany, ...) {
  cmap <- colorRampPalette(c(...))
  scols <- col2rgb(cmap(howmany))
  apply(scols, 2, function(c) rgb(c[1]/255, c[2]/255, c[3]/255))
}

plot_temp_map <- function(
  r, title,
  colorscale_title = "",
  contour_thresh = NULL,
  contour_quantiles = NULL,
  contour_col = c("blue", "red"),
  ctable = interpolate_colortable(howmany = 100,
                                  "blue", "green", "yellow", "red"),
  topn = 0, bottomn = 0)
{
  if (!is.null(contour_quantiles)) {
    contour_thresh <- quantile(r, probs = contour_quantiles, na.rm = TRUE)
  }

  alpha_of_map <- ifelse( is.null(contour_thresh), 1, .4)

  # plot
  par(mar = c(4,5,4,2) + .1)
  plot(r, col = alpha(ctable, alpha_of_map),
       axes = FALSE,
       #main = title,
       legend.args = list(text = colorscale_title,
                          side = 3, font = 2, line = 0, cex = 0.8)
  )

  # top-order
  xyFromCell(r, 1:ncell(r)) %>%
    as.data.frame %>%
    dplyr::select(lon = x, lat = y) %>%
    mutate(score = values(r)) -> XY

  if (topn > 0) {
    XY %>% arrange(-score) %>% head(topn) %>%
      points(pch = "O", cex = 3, col = "#aa0000")
  }

  if (bottomn > 0) {
    XY %>% arrange(score) %>% head(bottomn) %>%
      points(pch = "O", cex = 3, col = "#0000aa")
  }

  # contours + legend
  if (!is.null(contour_thresh)) {

    if ( all(!is.na(contour_thresh)) ) {
      contour(r, lwd = 2, add = TRUE, levels = contour_thresh, col = contour_col)
    }

    e <- extent(r)
    legend(e@xmax, e@ymin,
           xjust = 1,
           yjust = 0,
           bg = rgb(1, 1, 1, 0.7),
           legend = c(sprintf("cold island (%g)", round(contour_thresh[1], 2)),
                      sprintf("heat island (%g)", round(contour_thresh[2], 2))
                      ),
           fill = contour_col)
  }

  if (topn != 0 || bottomn != 0) {
    mtext("Min/Max value indicated by circle", cex = .8)
  }
}

plot_zscore <- function(
  r, sigma_thresh = 5,
  hotspot_quantile_thresh = .01,
  title = NULL)
{

  hotspots <- quantile(r, na.rm = TRUE,
                       probs = c(hotspot_quantile_thresh,
                                1 - hotspot_quantile_thresh))

  par(mar = c(4,5,4,2) + .1)
  plot(r, axes = FALSE, alpha = .7,
       col = gray.colors(100),
       legend.args = list(text = "z-score",
                          side = 3, font = 2, line = 0, cex = 0.8))

  rth <- r
  rth[abs(r) < sigma_thresh] <- NA
  plot(rth, axes = FALSE, alpha = .2, add = TRUE,
       col = interpolate_colortable(howmany = 100, "blue", "#FF3300"))

  # contours + legend
  contour(rth, lwd = 4, add = TRUE, levels = hotspots, col = c("blue", "red"))

  e <- extent(r)
  legend(e@xmax, e@ymin,
         xjust = 1,
         yjust = 0,
         bg = rgb(1, 1, 1, 0.85),
         legend = c(sprintf("top %g%% quantile hotspots (%g)",
                            round( hotspot_quantile_thresh * 100, 2),
                            round( hotspots[2], 2)),
                    sprintf("bottom %g%% quantile hotspots (%g)",
                            round( hotspot_quantile_thresh * 100, 2),
                            round( hotspots[1], 2)) ),
         fill = c("red", "blue"))

  if (!is.null(title)) {
    mtext(title, cex = .8)
  }
}

filter_qthresh <- function(r, sigma = 5, quant_thresh = 0.01) {
  qval <- quantile(r, na.rm = TRUE, probs = 1 - quant_thresh)
  minval <- max(sigma, qval[1])
  r > minval
}

eval_parent_child_dir <- function(r_child, r_parent) {
  # sanity checks
  stopifnot(is(r_child, "RasterLayer"))
  stopifnot(is(r_parent, "RasterLayer"))
  stopifnot(is.logical(values(r_child)))
  stopifnot(is.logical(values(r_parent)))

  # C - P
  r_child_clump <- suppressWarnings(clump(r_child))
  rdiff <- r_child_clump
  suppressWarnings(rdiff[r_parent] <- NA)
  1 - length(unique(rdiff)) / length(unique(r_child_clump))
}

# TODO: child is computed twice - memoization might help
eval_getisord <- function(
  myraster, st, interval,
  wgen = function(wsize) weight_matrix_circular_fade(wsize, wsize/4)
) {

  results <- c()
  r_parent <- NULL

  pb <- txtProgressBar(max = max(interval), style = 3)
  for (wsize in interval) {

    setTxtProgressBar(pb, wsize)

    r_child <- r_parent
    r_parent <- filter_qthresh(
      GetisOrd(myraster, wgen(wsize), st)
    )

    # first iteration
    if (is.null(r_child)) next

    m <- eval_parent_child_dir(r_child, r_parent)
    results <- rbind(results, c(wsize, m))
  }
  close(pb)
  return(results)
}

# Weight matrix ----------------------------------------------------------------

plot_weight_matrix <- function(w, show.legend = TRUE) {
  # sanity check
  stopifnot(is.matrix(w))
  stopifnot(is.logical(show.legend))

  col <- gray.colors(100)
  image(w, col = col, axes = FALSE, asp = 1)

  if (show.legend) {
    # matrix dimensions
    legend(1,1,
           xjust = 1,
           yjust = 1,
           bg = rgb(0,0,0,.3),
           border = rgb(1,1,1,.5),
           text.col = "white",
           legend = c( sprintf("%d x %d", nrow(w), ncol(w)) )
           )

    # min/max values
    legend(1,0,
           xjust = 1,
           yjust = 0,
           bg = "white",
           legend = c(sprintf("Min value = %.2f", round(min(w, na.rm = TRUE), 2)),
                      sprintf("Max value = %.2f", round(max(w, na.rm = TRUE), 2))
           ),
           fill = c(first(col), last(col))
          )
  }
}

#' Just a simple square matrix.
#' @param size Odd number representing the matrix width and height.
#'   Minimal matrix size is 3x3.
#' @return boolean square matrix
#' @example
#' weight_matrix_squared(5)
#' @export
weight_matrix_squared <- function(size) {
  # sanity checks
  stopifnot(size %% 2 == 1) # size must be an odd number
  stopifnot(size >= 3)

  wmatrix <- matrix(TRUE, size, size)
  stopifnot(is.logical(wmatrix)) # sanity check
  return(wmatrix)
}

#' Circular shape with a sharp falloff at the edge.
#' @param size Odd number representing the matrix width and height.
#'   Minimal matrix size is 3x3.
#' @return boolean square matrix
#' @example
#' weight_matrix_circular(5)
#' plot_weight_matrix(weight_matrix_circular(2))
#' @export
weight_matrix_circular <- function(size) {
  # sanity checks
  stopifnot(size %% 2 == 1) # size must be an odd number
  stopifnot(size >= 3)

  w <- matrix(1, size, size)
  x <- col(w)
  y <- row(w)
  m <- (size + 1) / 2
  r <- sqrt((x - m) ^ 2 + (y - m) ^ 2)

  wmatrix <- (r <= m - 0.6)
  wmatrix[!wmatrix] <- NA # replace FALSE with NAs
  stopifnot(is.logical(wmatrix)) # sanity check
  wmatrix
}

#' Helper function used in \code{sigmoid_falloff}. See example.
#' @param x X-coordinate.
#' @return value on the sigmoid curve
#' @example
#' plot(sigmoid(-10:10), type="l")
sigmoid <- function(x) 1/(1 + exp(-x))

#' Helper function which produces a smooth transition from 1 to 0 with a
#' sigmoidal falloff. See example.
#' @param x
#' @param size Size of the
#' @param fsize
#' @example
#' x <- 1:100
#' y <- sigmoid_falloff(x, 100, 20)
#' plot(x, y)
#' @export
sigmoid_falloff <- function(x, size, fsize) {
  sigmoid( -6 * (x - size + fsize/2) / fsize)
}

#' Circular shape with sigmoid falloff at the edge.
#' @param size Odd number representing the matrix width and height.
#'   Minimal matrix size is 3x3.
#' @return square matrix of doubles
#' @example
#' weight_matrix_circular_fade(5,2)
#' plot_weight_matrix(weight_matrix_circular_fade(11,2))
#' @export
weight_matrix_circular_fade <- function(size, fsize) {
  # sanity checks
  stopifnot(size %% 2 == 1) # size must be an odd number
  stopifnot(size >= 3)

  w <- matrix(1, size, size)
  x <- col(w)
  y <- row(w)
  m <- (size + 1) / 2
  r <- sqrt((x - m) ^ 2 + (y - m) ^ 2)

  wmatrix <- sigmoid_falloff(r, size/2, fsize)
  wmatrix[wmatrix == 0] <- NA
  stopifnot(is.double(wmatrix)) # sanity check
  wmatrix
}

# G* statistics ----------------------------------------------------------------

#' Focal convolution of a raster using a weight matrix.
#' Data on the border are padded with the mean value during convolution.
#'
#' @param r raster
#' @param w square weight matrix with odd number of rows/columns
#' @return convoluted raster
#' @examples
#' r <- raster(matrix(rnorm(10000), 100, 100))
#' w <- weight_matrix_circular(9)
#' plot(JustConvolve(r, w))
#' @export
JustConvolve <- function(r, w) {
  # sanity checks
  stopifnot(is(r, "RasterLayer"))
  stopifnot(is.matrix(w))

  MPAD <- mean(values(r))
  focal(r, w = w, pad = TRUE, padValue = MPAD, fun = sum)
}

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

#' Standard Getis-Ord G* statistics.
#'
#' @param r Input raster layer
#' @return Returns a list containing:
#' \item{rmean}{Global mean of all raster cells.}
#' \item{n}{Number of all cells within the raster.}
#' \item{SD}{Standard deviation of all raster cells (single value)}
#' \item{MEAN}{Same as rmean in standard Getis-Ord}
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

# Experiment
# w <- matrix(c(0,1,0,
#               1,1,1,
#               0,1,0), 3,3)
# wNA <- w
# wNA[w == 0] <- NA
#
# r <- raster(matrix(1,7,7))
# r[3,3] <- NA
# as.matrix(r)
# n <- focal(r, wNA,
#            fun = function(x) sum(!is.na(x)),
#            pad = TRUE, padValue = 1)
# as.matrix(n)
#
