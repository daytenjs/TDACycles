#' Plot a cycle on the supplied data given by the cycle segments
#' @param data Random data from the experiment, must be only data coordinates
#' (i.e., "x", "y", "z") along with an id for the cycle as the first column.
#' @param ordered_cycles The ordered cycles provided by cycle_extract().
#' @param plot_data Boolean to toggle plotting underlying data
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
#' require(dplyr)
#' rr <- ripsgen()
#' plot_cycle(
#'   data = rr$data,
#'   ordered_cycles = cycle_extract(
#'     diagram = rr$diagram,
#'     cycle_segments = rr$cycle_segments
#'   )
#' )
#'
#' # you can also plot one cycle at a time if you loop the following appropriately
#' cycle <- cycle_extract(
#'   diagram = rr$diagram,
#'   cycle_segments = rr$cycle_segments
#' ) %>% filter(cycle_ix == 31)
#'
plot_cycle <- function(data, ordered_cycles, plot_data = TRUE) {
  if (missing(data) | missing(ordered_cycles)) {
    print("Missing data or cycle_segments or both. Plotting generic data placeholder.")
    rr <- ripsgen()
    data <- rr$data
    ordered_cycles <- cycle_extract(diagram = rr$diagram, cycle_segments = rr$cycle_segments)
  }

  if(plot_data){
    plot(data)
  }
  ocs <- ordered_cycles
  for (jj in unique(ocs[,1])) {
    cycle <- ocs[ocs[,1]==jj, ]
    n <- dim(cycle)[1]
    for (ii in 1:n) {
      v <- cycle[ii, ]
      if (ii == n) {
        w <- cycle[1, ]
      } else {
        w <- cycle[ii + 1, ]
      }
      segments(v[, 2], v[, 3], w[, 2], w[, 3], col = jj)
    }
  }
  return(NA)
}
