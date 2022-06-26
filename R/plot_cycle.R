#' Plot a cycle on the supplied data given by the cycle segments
#' @param data Random data from the experiment, must be only data coordinates (i.e., "x", "y", "z")
#' @param ordered_cycles The ordered cycles provided by cycle_extract().
#' @return A plot
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom rlang .data
#' @importFrom graphics segments
#'
#' @export
#'
#' @examples
#' rr <- ripsgen()
#' plot_cycle(
#'   data = rr$data,
#'   ordered_cycles = cycle_extract(diagram = rr$diagram, cycles = rr$cycle_segments)
#' )
plot_cycle <- function(data, ordered_cycles) {
  if (missing(data) | missing(ordered_cycles)) {
    print("Missing data or cycle_segments or both. Plotting generic data placeholder.")
    rr <- ripsgen()
    data <- rr$data
    ordered_cycles <- cycle_extract(diagram = rr$diagram, cycles = rr$cycle_segments)
  }

  plot(data)
  ocs <- ordered_cycles
  for (jj in unique(ocs$cycle_ix)) {
    cycle <- ocs %>% filter(.data$cycle_ix == jj)
    n <- dim(cycle)[1]
    for (ii in 1:n) {
      v <- cycle[ii, ]
      if (ii == n) {
        w <- cycle[1, ]
      } else {
        w <- cycle[ii + 1, ]
      }
      segments(v[["x1"]], v[["x2"]], w[["x1"]], w[["x2"]], col = jj)
    }
  }
  return(NA)
}
