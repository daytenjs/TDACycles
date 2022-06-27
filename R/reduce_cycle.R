#' Algorithm that returns a data.frame of the ordered
#'
#' @param ordered_cycle Must be only data coordinates (i.e., "x", "y", "z")
#' in a sequential (adjacent) order.
#' @param og_ix The index of the cycle being passed.
#' @param cycle_birth Birth epsilon of original cycle.
#'
#' @return A data.frame identified by the original cycle index it is defined upon.
#' It shows the steps taken to reduce a given cycle by either shrinking a representative cycle
#' by an edge that cuts off a triangle from the original cycle, or by splitting a cycle
#' into two cycles with 4 or more points per cycle (they will share an edge), and giving then a
#' new index \code{ix}. It tracks hierarchy/order by \code{pix}, tells you whether a given step
#' for a given cycle caused a split, lists the points in the new cycle, the epsilon
#' (regarding the Rips Complex) at which value the shrink/split occured, and whether or not
#' this moved caused the death of the homological class (whether this new cycle is just a triangle).
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr tally
#' @importFrom dplyr group_by
#' @importFrom dplyr select
#' @importFrom dplyr pull
#' @importFrom rlang .data
#' @importFrom stats var
#'
#' @export
#'
#' @examples
#' require(dplyr)
#'
#' rr <- ripsgen(n = 60)
#' diagram <- rr$diagram
#' ordered_cycles <- cycle_extract(
#'   diagram = rr$diagram,
#'   cycle_segments = rr$cycle_segments
#' )
#'
#' cyc_edge_count <- ordered_cycles %>%
#'   group_by(cycle_ix) %>%
#'   tally()
#' og_ix <- cyc_edge_count[which.max(cyc_edge_count$n), "cycle_ix"][[1]]
#'
#' ordered_cycle <- ordered_cycles %>% filter(cycle_ix == og_ix)
#' cycle_birth <- diagram[og_ix, "Birth"]
#' max_iter <- 10
#'
#' reduced_mat <- reduce_cycle(ordered_cycle, og_ix, cycle_birth[[1]])
#'
#' plot_cycle(data = rr$data, ordered_cycles = reduced_mat %>% select(ix, x, y))
#'
#'
#'
#' ### You can also animate the reduction
#' par(mar = c(4.5, 4.5, 2.5, 1.5))
#' plot(rr$data, xlab = "x", ylab = "y", main = "Sample (Rudimentary) Animation")
#' for (i in 1:23) {
#'   plot_cycle(
#'     rr$data,
#'     reduced_mat %>%
#'       filter(ix == i) %>%
#'       select(ix, x, y),
#'     plot_data = FALSE
#'   )
#'   if (i < 7) {
#'     Sys.sleep(1.3)
#'   } else {
#'     Sys.sleep(0.4)
#'   }
#' }
#' plot_cycle(rr$data, reduced_mat %>% filter(ix == 1) %>% select(ix, x, y), plot_data = FALSE)
#'
reduce_cycle <- function(ordered_cycle, og_ix, cycle_birth) {
  if (missing(ordered_cycle)) {
    return("Error, ordered_cycle not supplied")
  }
  if (missing(og_ix)) {
    return("Error, og_ix not supplied")
  }
  if (missing(cycle_birth)) {
    return("Error, cycle_birth not supplied")
  }
  reduce_mat <- as.data.frame(matrix(nrow = 0, ncol = 9))
  cols <- c(
    "og_ix",
    "step",
    "ix",
    "pix",
    "from_split",
    "x",
    "y",
    "epsilon",
    "is_death"
  )
  colnames(reduce_mat) <- cols

  # initialize original cycle in reduce_mat
  reduce_mat <- rbind(
    reduce_mat,
    cbind(
      "og_ix" = og_ix,
      "step" = 0,
      "ix" = 1,
      "pix" = NA,
      "from_split" = FALSE,
      "x" = ordered_cycle$x1,
      "y" = ordered_cycle$x2,
      "epsilon" = cycle_birth,
      "is_death" = FALSE
    )
  )

  ### Loop Params
  fully_reduced <- FALSE # loop controller for "break"
  this_step <- 0 # iterator for filtering

  ### CYCLE REDUCER
  # take cyc(i) and get edge to remove if length > 3
  # get cyc(i+1) and cyc(i+2) and write to reduce_mat (shrink or split)
  # iterate until all cyc(k_i) length == 3
  while (!fully_reduced) {
    step_mat <- reduce_mat %>% filter(.data$step == this_step)
    ixs <- step_mat %>%
      pull(.data$ix) %>%
      unique()

    this_step <- this_step + 1
    step_mat$ix %>% unique()
    this_ix <- 7
    for (this_ix in step_mat$ix %>% unique()) {
      cycle <- step_mat %>% filter(.data$ix == this_ix)

      if (dim(cycle)[1] == 3) {
        print("NEXT")
        next
      }

      # retrieve adjacency matrix, indices of edge to reduce, and epsilon value
      result <- select_edge(cycle)
      indices <- result$edge
      eps <- result$epsilon

      n <- dim(cycle)[1]
      cyc1 <- cycle[indices[1]:indices[2], ]
      cyc2 <- cycle[c(1:indices[1], indices[2]:n), ]

      reduce_mat <- rbind(
        reduce_mat,
        cbind(
          "og_ix" = og_ix,
          "step" = this_step,
          "ix" = max(ixs) + 1,
          "pix" = this_ix,
          "from_split" = FALSE,
          "x" = cyc1$x,
          "y" = cyc1$y,
          "epsilon" = eps,
          "is_death" = dim(cyc1)[1] == 3
        ),
        cbind(
          "og_ix" = og_ix,
          "step" = this_step,
          "ix" = max(ixs) + 2,
          "pix" = this_ix,
          "from_split" = FALSE,
          "x" = cyc2$x,
          "y" = cyc2$y,
          "epsilon" = eps,
          "is_death" = dim(cyc2)[1] == 3
        )
      ) %>% as.data.frame()
    }

    step_cycles_lengths <- reduce_mat %>%
      filter(.data$step == this_step) %>%
      group_by(.data$ix) %>%
      tally() %>%
      pull(n)

    fully_reduced <- var(step_cycles_lengths) %in% 0
  }
  return(reduce_mat)
}
