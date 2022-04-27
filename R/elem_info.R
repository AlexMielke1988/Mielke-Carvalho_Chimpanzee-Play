#' Extract information on Antecedent -> consequent transitions. Can be quantified in different ways (sum of transitions, conditional probability, joint probability, point-wise mutual information)
#'
#' @param antecedent first element in transition
#' @param consequent second element in transition
#' @param elem.bout list of vectors of elements. Co-occurring elements are marked by the percentage symbol.
#' @param it number of iterations of assignments
#' @param measure which measure to quantify the antecedent - consequent transition probability: 'sum' gives the number of transitions, 'prob' gives conditional probability, 'joint.prob' gives joint probability, 'mi' gives point-wise mutual information
#' @param ran.method either 'sample' (just sample one of the co-occurring) or 'random' (randomizes their order)
#'
#' @return Returns a dataframe with the antecedents, consequents and the information about the transitions
#'
#' @importFrom dplyr mutate arrange desc as_tibble select all_of na_if inner_join left_join bind_rows bind_cols rename mutate_all distinct row_number summarize tibble filter count distinct
#' @importFrom stringr str_detect str_split str_replace_all str_to_title str_to_lower
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na gather separate unite
#'
#' @author Alex Mielke
#' @export
#'

elem_info <- function(antecedent,
                      consequent,
                      elem.bout,
                      it,
                      measure = c("sum", "prob", "mi", "joint.prob"),
                      ran.method = 'random') {

  # create matrices of transitions
  element.bout.sum <- data.frame(
    elem_bout_matrix(elem.bout = elem.bout,
                          it = it,
                          ran.method = ran.method),
    check.names = F)

  # calculate co-occurrences
  AandB <- unlist(sapply(1:length(antecedent), function(x) {
    if (antecedent[x] %in% rownames(element.bout.sum) &
        consequent[x] %in% colnames(element.bout.sum)) {
      xx <- element.bout.sum[
        antecedent[x], consequent[x]
      ]
    }
    if (!(antecedent[x] %in% rownames(element.bout.sum)) |
        !(consequent[x] %in% colnames(element.bout.sum))) {
      xx <- 0
    }
    return(xx)
  }))

  # calculate prob of elements occurring on their own
  Aall <- rowSums(element.bout.sum)[match(antecedent, as.character(rownames(element.bout.sum)))]
  Ball <- colSums(element.bout.sum)[match(consequent, as.character(colnames(element.bout.sum)))]

  Aall <- ifelse(is.na(Aall), 0, Aall)
  Ball <- ifelse(is.na(Ball), 0, Ball)

  # calculate probability of either or neither occurring
  AnotB <- Aall - AandB
  BnotA <- Ball - AandB
  notAnotB <- sum(element.bout.sum) - Ball - Aall

  #return measures
  if (measure == "sum") {
    return(AandB)
  }
  if (measure == "joint.prob") {
    return(AandB / sum(element.bout.sum))
  }
  if (measure == "prob") {
    return(AandB / Aall)
  }
  if (measure == "mi") {
    return(
      (log(
        (AandB / sum(element.bout.sum)) /
          (Aall / sum(element.bout.sum) *
             Ball / sum(element.bout.sum))
      )
      ) /
        (-1 * log(AandB / sum(element.bout.sum))
        )
    )
  }
}
