#' Evaluation metric (child -> parent)
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
