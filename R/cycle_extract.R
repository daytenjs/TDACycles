#' Extract cycles into a better format from ripsgen() function
#' @param n Number of points to generate (required if "dat" is not supplied).
#' @param dim The underlying ambient dimension of the result, i.e., dim=2 yields
#' data in a a unit square (required if "dat" is not supplied).
#' @param diagram The Rips diagram as returned by ripsgen()$diagram.
#' @param cycles The representative cycles as returned by ripsgen()$cycles.
#' @return something here
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' 2+2
cycle_extract <- function(n = 30, dim = 2, diagram = NA, cycles = NA) {
  if (is.na(diagram)) {
    diagram <- ripsgen()$diagram
  }

  diagram <- diagram[] %>% as.data.frame() # just in case still in S3 class type

  if (is.na(cycles)) {
    cycles <- ripsgen()$cycles
  }

  ordered_cycles <- matrix(NA, nrow = 0, ncol = dim + 1)
  colnames(ordered_cycles) <- c("ix", paste0("x", 1:dim))

  one <- which(diagram == 1)

  for (ii in seq(along = one)) {
    cycle <- cycles %>% filter(.data$ix == ii)
    ordered_cycle <- matrix(NA, nrow = dim(cycle)[1], ncol = 2)
    v1 <- cycle[1, 1:2]
    v2 <- cycle[1, 3:4]
    ordered_cycle[1, ] <- unlist(v1)
    ordered_cycle[2, ] <- unlist(v2)
    cycle <- cycle[-1, ]

    for (ii in 3:(dim(cycle)[1] + 1)) {
      opt1 <- cycle %>% filter(.data$x0 == ordered_cycle[ii - 1, 1]
                               & .data$y0 == ordered_cycle[ii - 1, 2])
      opt2 <- cycle %>% filter(.data$x1 == ordered_cycle[ii - 1, 1]
                               & .data$y1 == ordered_cycle[ii - 1, 2])
      if (dim(opt1)[1] == 0) {
        ordered_cycle[ii, ] <- opt2 %>%
          select(.data$x0, .data$y0) %>%
          unlist()
        ix_to_null <- opt2$ix
      } else {
        ordered_cycle[ii, ] <- opt1 %>%
          select(.data$x1, .data$y1) %>%
          unlist()
        ix_to_null <- opt1$ix
      }
      cycle[ix_to_null, ] <- -1
    }

    ordered_cycles <- rbind(ordered_cycles, cbind(ii, ordered_cycle))
  }






  return(NA)
}
