# TODO: cleanup and split into multiple files

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



