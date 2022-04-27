#' Take sequence within bout and randomize sequence, keeping the number of elements the same and either controlling for the overall probability across bouts, or control the elements within bouts
#'
#' The \code{\link{randomize_bouts}} takes the lists of bouts and randomizes the elements while keeping the number of elements per bout
#'
#' @param elem.bout list of vectors of elements. Co-occurring elements are marked by the percentage symbol. Function returns same vector, but with co-occurring elements split.
#' @param type how to determine probability of elements for randomization. If 'across', frequencies of elements are the same across all bouts, while 'within' shuffles elements within a bout
#' @param unlisting should output be unlisted or retain original list structure?
#'
#' @return Function returns a list of vectors with the randomized, split elements
#'
#' @importFrom dplyr mutate arrange desc as_tibble select all_of na_if inner_join left_join bind_rows bind_cols rename mutate_all distinct row_number summarize tibble filter count distinct
#' @importFrom stringr str_detect str_split str_replace_all str_to_title str_to_lower
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na gather separate unite
#'
#' @author Alex Mielke
#' @export
#'
randomize_bouts <- function(elem.bout,
                            type = "across",
                            unlisting = T) {
  # 'across' shuffles the elements across all bouts while keeping number of elements per bout the same
  if (type == "across") {
    # unlist the elem.bout list
    elem.unlist <- unlist_list(elem.bout = elem.bout, method = "random")
    # if no unlisting wanted, go back
    if (!unlisting) {
      elem.unlist <- elem.bout
    }
    # give list numbers
    names(elem.unlist) <- 1:length(elem.unlist)
    # completely unlist list
    xx <- unlist(elem.unlist, recursive = F, use.names = F)
    # sample everything apart from breaks and NAs
    xx[xx != "Break" &
      !is.na(xx)] <- sample(xx[xx != "Break" & !is.na(xx)])
    # add info of which bout things belong to back to xx
    yy <- unlist(sapply(1:length(elem.unlist), function(x) {
      rep(names(elem.unlist)[x], length(elem.unlist[[x]]))
    }),
    recursive = F,
    use.names = F
    )
    yy <- lapply(unique(yy), function(x) {
      xx[yy == x]
    })
  }

  # if type is 'within', elements are shuffled within bouts
  if (type == "within") {
    yy <- lapply(elem.bout, function(x) {
      x.e <- unlist_vector(x, method = "random")
      if (!unlisting) {
        x.e <- x
      }
      # keep 'Break' and NA the same
      x.e[x.e != "Break" &
        !is.na(x.e)] <- sample(x.e[x.e != "Break" & !is.na(x.e)])
      return(x.e)
    })
  }
  return(yy)
}
