#' Take sequence within bout and randomly assign sequence for co-occurring elements (marked by percentage symbol)
#'
#' Takes the vector consisting of elements in their order of occurrence and unlists elements that occur at the same time, randomly.
#'
#' @param bout vector of elements. Co-occurring elements are marked by the percentage symbol. Function returns same vector, but with co-occurring elements split.
#' @param method either 'sample' (just sample one of the co-occurring) or 'random' (randomizes their order)
#'
#' @return Function returns a vector with the split elements
#'
#' @importFrom stats sd quantile
#' @importFrom magrittr %>%
#'
#' @author Alex Mielke
#'
#' @export

unlist_vector <- function(bout, method = "sample") {
  # take every element, if they have a '%' included randomly assign order
  bout <- as.character(as.vector(unlist(bout)))
  v.l <- lapply(bout, function(x) {
    unlist(
      strsplit(as.character(x),
        split = "%", fixed = T
      ),
      recursive = FALSE,
      use.names = FALSE
    )
  })
  # if random, shuffle order
  if (method == "random") {
    v.l <- sapply(v.l, sample)
  }
  # if sample, take one
  if (method == "sample") {
    v.l <- sapply(v.l, sample, size = 1)
  }
  return(unlist(v.l))
}
