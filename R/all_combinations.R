#' Make all combinations of element vector
#'
#' @param elem.bout list of vectors of elements. Co-occurring elements are marked by the percentage symbol.
#'
#' @return data frame with all antecedent and consequent combinations
#'
#' @importFrom dplyr mutate arrange desc as_tibble select all_of na_if left_join bind_rows bind_cols rename mutate_all distinct row_number
#' @importFrom stringr str_detect str_split str_replace_all str_to_title str_to_lower
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na gather separate unite
#' @importFrom arrangements combinations
#'
#' @author Alex Mielke
#' @export
#'

all_combinations <- function(elem.bout){

  # create all elements in elem.bout and sort them
  x <- elem.bout %>%
    unlist(recursive = F, use.names = F) %>%
    strsplit(split = '%', fixed = T) %>%
    unlist(recursive = F, use.names = F) %>%
    unique() %>%
    na.omit() %>%
    sort()
  # bind all elements in frame with two columns
  y <-
    do.call(rbind,
      list(arrangements::combinations(
        x = x,
        k = 2,
        replace = T
      ),
      # redo with decreasing element to ensure every combination exists twice
      arrangements::combinations(
        x = sort(x, decreasing = T),
        k = 2,
        replace = T
      )
    )) %>%
    data.frame() %>%
    # remove duplicates
    distinct(.data$X1, .data$X2)

  colnames(y) = c('antecedent','consequent')
  return(y[order(y$antecedent,
                 y$consequent),])

}
