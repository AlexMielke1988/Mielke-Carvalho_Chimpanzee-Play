#' Take elements that occur together and remove one of them
#'
#' @param elem.bout vector of elements. Co-occurring elements are marked by the percentage symbol.
#' @param elements.to.change vector of element names to be changed, e.g. c('wave-arm','wiggle-arm','raise-arm')
#' @param new new name to give to those elements (e.g., 'move-arm')
#'
#' @return Function returns a list of vectors with renamed elements
#'
#'
#' @author Alex Mielke
#' @export
#'

combine_elements <-
  function(elem.bout,
           elements.to.change,
           new) {
    # go through each bout, unlist it, change the specified elements
    elem.bout <- lapply(elem.bout, function(x) {
      # split cases of co-occurring elements
      bout <- strsplit(x, split = "%", fixed = T)
      # unlist bouts, change elements, but them back together
      bout <- lapply(bout, function(y) {
        y <- unlist(y)
        if (sum(elements.to.change %in% y) != length(elements.to.change)) {
          return(y)
        }
        if (sum(elements.to.change %in% y) == length(elements.to.change)) {
          y <- c(y[!y %in% elements.to.change], new)
        }
      })
      bout <- sapply(bout, paste, collapse = "%")
      return(bout)
    })
    return(elem.bout)
  }
