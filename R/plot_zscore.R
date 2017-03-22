#' This function is suitable for plotting zscores encoded as a raster.
#' @author Viliam Simko
#'
#' @param r Raster to be plotted.
#' @param sigma_thresh Sigma (zscore) value used for filtering significant
#'   raster cells.
#' @param hotspot_quantile_thresh Quantile value used as a threshold for
#'   plotting hotspots.
#' @param title Title of the plot
#'
#' @importFrom raster quantile plot contour
#' @import grDevices
#' @export
plot_zscore <- function(
  r, sigma_thresh = 5,
  hotspot_quantile_thresh = .01,
  title = NULL,
  legend=TRUE)
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
  if(legend==TRUE){
    
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
    
  }
  if (!is.null(title)) {
    mtext(title, cex = .8)
  }
}
