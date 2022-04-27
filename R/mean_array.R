#' Helper functions that take a list of matrices and sums across them or takes the mean
#'
#' @param list.matr list of transition matrices
#'
#' @return mean.array returns a matrix with the mean across all individual matrices. sum.array returns a matrix with the sum across all individual matrices
#'
#'
#' @author Alex Mielke
#' @export
#'
#' @examples
#' ###
#'
#'
#' ## summarise array
mean_array <- function(list.matr) {
  Reduce("+", list.matr) / length(list.matr)
}
sum_array <- function(list.matr) {
  Reduce("+", list.matr)
}
