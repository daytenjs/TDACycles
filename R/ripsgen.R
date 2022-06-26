#' Generate Rips Complex for data
#' @param n Number of points to generate (required if "dat" is not supplied).
#' @param dim The underlying ambient dimension of the result, i.e., dim=2 yields
#' data in a a unit square (required if "dat" is not supplied).
#' @param dat An optional tibble, dataframe, or matrix, with columns of randomly distributed data only (i.e., "x", "y", and "z" being all random uniform variables).
#' @return A List with the original or randomly generated data, along with the Rips diagram, birth and death times, and cycles segments that can draw the cycles on the random data.
#'
#' @importFrom tidyr %>%
#' @export
#'
#' @examples
#' rr <- ripsgen(50, 2)
#' rr$diagram
ripsgen <- function(n = 30, dim = 2, dat = NA) {
  # TODO: Add typing (https://github.com/moodymudskipper/typed) and error handling

  if (is.na(dat)) {
    dat <- datagen(n, dim)
  } else {
    dim <- dim(dat)[2]
  }

  maxScale <- dat %>%
    lapply(range) %>%
    lapply(diff) %>%
    unlist()

  DiagRips <- TDA::ripsDiag(
    X = dat, maxdimension = (dim - 1), maxscale = sqrt(sum(maxScale)),
    library = "Dionysus", location = TRUE, printProgress = TRUE
  )

  pts_mat <- matrix(NA, ncol=5, nrow=0)
  one <- which(DiagRips[["diagram"]][, 1] == 1) #this allows to extract cycles

  for (jj in seq(along = one)) { #per cycle
    rips_cycle <- DiagRips[["cycleLocation"]][[one[jj]]] #extract cycle pts
    cycle_dim <- dim(rips_cycle[,1,])
    segments <- cbind(rips_cycle[,1,], rips_cycle[, 2, ])
    colnames(segments) <- c("x0", "y0", "x1", "y1")
    pts_mat <- rbind(pts_mat, cbind(one[jj], segments)) # store [cycle_ix, x, y]
  }

  return(
    list(
      data = dat,
      diagram = DiagRips$diagram,
      births = DiagRips$birthLocation,
      deaths = DiagRips$deathLocation,
      cycle_segments = pts_mat
    )
  )
}
