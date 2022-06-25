#' Generate randomly distributed data in an n-Cube.
#'
#' @param n Number of points to generate
#' @param dim The underlying ambient dimension of the result, i.e., dim=2 yields
#' data in a a unit square
#'
#' @return A tibble with "n" rows and "dim" columns of random data.
#' @export
#'
#' @examples
#' dat <- datagen(50, 3)
#' pairs(dat)
datagen <- function(n=30, dim=2) {
  dat <- tidyr::as_tibble(matrix(ncol = dim, nrow = n, NA))
  for(ii in 1:dim){
    dat[,ii] <- stats::runif(n, 0, 1)
  }
  return(dat)
}
