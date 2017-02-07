#' Evaluation metric (child -> parent)
#' @author Viliam Simko
#'
#' @param r_child Raster processed with a "smaller" weight matrix.
#' @param r_parent Raster processed with a "larger" weight matrix.
#' @return A number between 0 and 1
#'
#' @importFrom raster clump unique values
#' @export
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

#' Evaluation metric (child -> parent)
#' @author Viliam Simko
#'
#' @param r_child Raster processed with a "smaller" weight matrix.
#' @param r_parent Raster processed with a "larger" weight matrix.
#' @return A number between 0 and 1
#'
#' @importFrom raster clump unique values
#' @export
eval_child_parent_dir <- function(r_child, r_parent) {

  # sanity checks
  stopifnot(is(r_child, "RasterLayer"))
  stopifnot(is(r_parent, "RasterLayer"))
  stopifnot(is.logical(values(r_child)))
  stopifnot(is.logical(values(r_parent)))

  stop("Not yet implemented") # TODO: implement
}
