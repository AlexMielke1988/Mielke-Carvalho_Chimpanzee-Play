#' Change element names, allows you to put several elements together
#'
#' Because element names might be overlapping and tied up in co-occurrences, we can't just use str_replace, so everything has to be unlisted, elements renamed, and then put back together
#'
#' @param elem.bout vector of elements. Co-occurring elements are marked by the percentage symbol.
#' @param elements.to.change vector of element names to be changed, e.g. c('wave-arm','wiggle-arm','raise-arm')
#' @param new new name to give to those elements (e.g., 'move-arm')
#'
#' @return Function returns a list of vectors with renamed elements
#'
#' @author Alex Mielke
#' @export
#'
change_elements <- function(elem.bout,
                            elements.to.change,
                            new) {
  # go through every bout
  elem.bout <- lapply(elem.bout, function(x) {
    # go through every element in the bout
    for (j in 1:length(x)) {
      # unlist them where necessary
      xx <- unlist(strsplit(x[j], split = "%"))
      # rename
      xx[xx %in% elements.to.change] <- new
      # put back together
      if (length(xx) > 1) {
        xx <- paste(xx, collapse = "%")
      }
      x[j] <- xx
    }
    return(x)
  })
  return(elem.bout)
}
