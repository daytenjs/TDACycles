#' Plot a cycle on the supplied data given by the cycle segments
#' @param data Random data from the experiment, must be only data coordinates (i.e., "x", "y", "z")
#' @param cycle_segments The provided cycle segments, looks like "index", "x0", "y0", "x1", "y1".
#' @return A plot
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' rr <- ripsgen()
#' plot_cycle_segments(data = rr$data, cycle_segments = rr$cycle_segments)
plot_cycle_segments <- function(data, cycle_segments) {
  if (missing(data) | missing(cycle_segments)) {
    print("Missing data or cycle_segments or both. Plotting generic data placeholder.")
    rr <- ripsgen()
    data <- rr$data
    cycle_segments <- rr$cycle_segments
  }

  plot(data)
  cs <- cycle_segments
  for (jj in unique(cs$cycle_ix)) {
    cycle <- cs %>% filter(.data$cycle_ix == jj)
    for (ii in 1:dim(cycle)[1]) {
      graphics::segments(
        cycle[ii, "x0"],
        cycle[ii, "y0"],
        cycle[ii, "x1"],
        cycle[ii, "y1"],
        col = jj
      )
    }
  }
  return(NA)
}
