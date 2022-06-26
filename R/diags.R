#' Rips Diagrams
#'
#' Rips diagrams from 297 different datasets ranging from 3 to 300 points.
#' To be used in conjuction with "pnts" and "cycs" datasets.
#'
#' @format A data frame with 54802 rows and 6 columns
#' \describe{
#'   \item{run}{Dataset Identifier}
#'   \item{cycle_ix}{Identifier for a homology class}
#'   \item{npts}{Number of points in the dataset}
#'   \item{dim}{Homological Dimension}
#'   \item{birth}{Initial epsilon value}
#'   \item{death}{Initial epsilon value}
#' }
"diags"
