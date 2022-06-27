#' Find the edge to shrink or split a representative cycle
#' @param ordered_cycle Must be only data coordinates (i.e., "x", "y", "z")
#' in a sequential (adjacent) order.
#' @return List of the adjacency matrix, the indices of the edge to split or
#' shrink upon, and the epsilon value this occurs at.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' 2 + 2 # TODO
select_edge <- function(ordered_cycle) {
  if (missing(ordered_cycle)) {
    return("Error, ordered_cycle not supplied")
  }

  n <- dim(ordered_cycle)[1]

  adjacency <- stats::dist(ordered_cycle, upper = FALSE) %>%
    as.matrix()

  adjacency[lower.tri(adjacency)] <- Inf # only need upper triangle
  diag(adjacency) <- Inf # using min, so overwrite diagonal
  adjacency[1, n] <- Inf # these edges are ALSO adjacent in the cycle, overwrite
  for (i in 1:(n - 1)) {
    adjacency[i, i + 1] <- Inf
  }
  edge <- which(adjacency == min(adjacency), arr.ind = TRUE)[1, ] %>% sort()
  return(list(adjacency = adjacency, edge = edge, epsilon = min(adjacency)))
}
