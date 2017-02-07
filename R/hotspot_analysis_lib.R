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

#' Plots a color table (useful for debugging).
#' Uses \link{raster} for plotting.
#'
#' @param ct Vector of colors
#' @examples
#' plot_color_table(c("black", "red", "gold")) # German flag
#' @export
plot_color_table <- function(ct) {
  x <- raster(nrows = length(ct), ncols = 1, vals = 0:(length(ct) - 1))
  par(mar = c(0,1,0,1))
  plot(x, col = ct, axes = FALSE, box = FALSE)
}

#' Helper functon to generates a color table.
#' @param howmany Number of elements of the resulting vector.
#' @param ... A vecor of colors to be used for interpolation.
#' @return Returns a vector representing the color table.
#' @examples
#' ct <- interpolate_colortable(10, "red", "green" ,"blue")
#' plot_color_table(ct)
#' @export
interpolate_colortable <- function(howmany, ...) {
  cmap <- colorRampPalette(c(...))
  scols <- col2rgb(cmap(howmany))
  apply(scols, 2, function(c) rgb(c[1]/255, c[2]/255, c[3]/255))
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
