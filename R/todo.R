#' @noRd
#' @note TODO: child is computed twice - memoization might help
eval_getisord <- function(
  myraster, st, interval,
  wgen = function(wsize) weight_matrix_circular_fade(wsize, wsize/4)
) {

  results <- c()
  r_parent <- NULL

  filter_qthresh <- function(r, sigma = 5, quant_thresh = 0.01) {
    qval <- quantile(r, na.rm = TRUE, probs = 1 - quant_thresh)
    minval <- max(sigma, qval[1])
    r > minval
  }

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
