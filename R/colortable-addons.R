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

#' Plots a color table (useful for debugging).
#' Uses \link{raster} for plotting.
#'
#' @param ct Vector of colors
#' @examples
#' plot_color_table(c("black", "red", "gold")) # German flag
#'
#' @importFrom raster raster plot
#' @export
plot_color_table <- function(ct) {
  x <- raster(nrows = length(ct), ncols = 1, vals = 0:(length(ct) - 1))
  par(mar = c(0,1,0,1))
  plot(x, col = ct, axes = FALSE, box = FALSE)
}
