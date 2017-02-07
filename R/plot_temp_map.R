#' This function is suitable for plotting raw temperatures encoded as a raster.
#'
#' @param r Input raster containing temperature values
#' @param title Title for the plot
#' @param colorscale_title Title for the color legend
#'
#' @import dplyr
#' @importFrom raster xyFromCell extent clump ncell values
#' @importFrom scales alpha
#'
#' @examples
#' r <- raster(matrix(rnorm(400), 20))
#' plot_temp_map(r, "Random temperatures")
#'
#' @export
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
  # TODO: add sanity checks for all parameters

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

  # TODO: the "score" variable
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
