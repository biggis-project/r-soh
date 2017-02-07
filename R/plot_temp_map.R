#' This function is suitable for plotting raw temperatures encoded as a raster.
#' @author Viliam Simko
#'
#' @param r Input raster containing temperature values
#' @param title Title for the plot
#' @param colorscale_title Title for the color legend
#' @param contour_thresh Values used for plotting contours.
#' @param contour_col Two colors representing min and max thresholds.
#' @param ctable Color table used for plotting the temperatures.
#'
#' @examples
#' r <- raster(matrix(rnorm(400), 20))
#' plot_temp_map(r, "Random temperatures")
#'
#' @importFrom graphics contour image legend mtext points rect
#' @importFrom raster extent clump ncell values
#' @importFrom scales alpha
#' @export
plot_temp_map <- function(
  r, title,
  colorscale_title = "",
  contour_thresh = NULL,
  contour_col = c("blue", "red"),
  ctable = interpolate_colortable(howmany = 100,
                                  "blue", "green", "yellow", "red")
  )
{
  # TODO: add sanity checks for all parameters

  alpha_of_map <- ifelse( is.null(contour_thresh), 1, .4)

  # plot
  par(mar = c(4,5,4,2) + .1)
  plot(r, col = alpha(ctable, alpha_of_map),
       axes = FALSE,
       #main = title,
       legend.args = list(text = colorscale_title,
                          side = 3, font = 2, line = 0, cex = 0.8)
  )

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
}
