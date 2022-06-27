#' Extract cycles into a better format from ripsgen() function
#' @param n Number of points to generate (required if "dat" is not supplied).
#' @param dim The underlying ambient dimension of the result, i.e., dim=2 yields
#' data in a a unit square (required if "dat" is not supplied).
#' @param diagram The Rips diagram as returned by ripsgen()$diagram.
#' @param cycle_segments The representative cycle_segments as returned by ripsgen()$cycle_segments.
#' @return Better formatted cycles
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom rlang .data
#'
#' @examples
#' require(dplyr)
#' diagram <- diags %>%
#'   filter(run == 28) %>%
#'   select(-c(run, cycle_ix, npts))
#'
#' cycle_segments <- cycs %>%
#'   filter(run == 28) %>%
#'   select(-c(run, npts))
#'
#' cycle_extract(diagram, cycle_segments)
#' @export
cycle_extract <- function(diagram, cycle_segments, n = 30, dim = 2) {
  if (missing(diagram)) {
    diagram <- ripsgen()$diagram
  }

  diagram <- diagram[] %>% as.data.frame() # just in case still an S3 class

  if (missing(cycle_segments)) {
    cycle_segments <- ripsgen()$cycle_segments
  }

  ordered_cycles <- matrix(NA, nrow = 0, ncol = (dim + 1))
  colnames(ordered_cycles) <- c("cycle_ix", paste0("x", 1:dim))

  one <- which(diagram$dim == 1)

  for (jj in seq(along = one)) {
    cycle <- cycle_segments %>%
      filter(.data$cycle_ix == one[jj]) %>%
      select(.data$x0, .data$y0, .data$x1, .data$y1)
    cycle$order <- 1:dim(cycle)[1] - 1

    ordered_cycle <- matrix(NA, nrow = dim(cycle)[1], ncol = 2)
    ordered_cycle[1, ] <- cycle[1, 1:2] %>% unlist()
    ordered_cycle[2, ] <- cycle[1, 3:4] %>% unlist()
    cycle <- cycle[-1, ]

    for (ii in 3:(dim(cycle)[1] + 1)) {
      opt1 <- cycle %>% filter(.data$x0 == ordered_cycle[ii - 1, 1] &
        .data$y0 == ordered_cycle[ii - 1, 2])
      opt2 <- cycle %>% filter(.data$x1 == ordered_cycle[ii - 1, 1] &
        .data$y1 == ordered_cycle[ii - 1, 2])

      if (dim(opt1)[1] == 0) {
        ordered_cycle[ii, ] <- opt2 %>%
          select(.data$x0, .data$y0) %>%
          unlist()
        ix_to_drop <- opt2$order
      } else {
        ordered_cycle[ii, ] <- opt1 %>%
          select(.data$x1, .data$y1) %>%
          unlist()
        ix_to_drop <- opt1$order
      }
      cycle <- cycle %>% filter(order != ix_to_drop)
    }

    ordered_cycles <- rbind(ordered_cycles, cbind(one[jj], ordered_cycle))
  }
  return(ordered_cycles %>% as.data.frame())
}
