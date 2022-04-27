#' Take list of sequences within bout and randomly assign sequence for co-occurring elements (marked by percentage symbol)
#'
#' Takes a list of the vector consisting of bouts of elements in their order of occurrence and unlists elements that occur at the same time, randomly.
#'
#' @param elem.bout list of bouts of elements. Co-occurring elements are marked by the percentage sign. Function returns same list of vectors, but with co-occurring elements split.
#' @param method either 'sample' (just sample one of the co-occurring); 'random' (randomizes their order); or 'optimal' (calculates probabilities outside of this case and assigns based on those)
#'
#' The resulting object is the basis for most other functions in this package.
#'
#' @return Function returns a list of vectors with the split elements
#'
#' @importFrom stats sd quantile
#' @importFrom magrittr %>%
#'
#'
#' @author Alex Mielke
#'
#' @export
#'

unlist_list <- function(elem.bout, method = 'sample') {
  return(sapply(elem.bout, function(y) {
    unlist_vector(y, method = method)
  }))
}

