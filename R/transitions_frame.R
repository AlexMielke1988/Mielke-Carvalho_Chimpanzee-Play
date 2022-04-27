#' Create dataframe that summarises how often each individual element occurs in the dataset; basis for many of the other functions
#'
#' @param elem.bout list of vectors of elements. Co-occurring elements are marked by the percentage symbol.
#' @param elements vector of all elements that should be included in the analysis; if 'NULL', the function takes all elements that occur in the elem.bout list
#' @param all.possible should output contain all possible combinations of elements, or only those that occurred at least 1 time?
#'
#' @return Function returns a data frame with each antecedent, consequent, and their individual occurrence stats
#'
#' @importFrom dplyr mutate arrange desc as_tibble select all_of na_if inner_join left_join bind_rows bind_cols rename mutate_all distinct row_number summarize tibble filter count distinct
#' @importFrom stringr str_detect str_split str_replace_all str_to_title str_to_lower
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na gather separate unite
#' @importFrom tidytext unnest_tokens
#' @importFrom tidyfast dt_separate
#' @importFrom purrr map
#'
#' @author Alex Mielke
#' @export
#'
#' @examples
#' ###
transitions_frame <- function(elem.bout,
                              elements = NULL,
                              all.possible = FALSE) {
  # unlist and clean element bout
  elem.bout <- elem.bout %>%
    map(str_replace_all,
        pattern = "-|/|_",
        replacement = ""
    )

  # if 'elements' is not specified, create vector with all unique elements
  if (is.null(elements)) {
    elements <-
      unique(unlist(
        strsplit(
          as.character(unlist(elem.bout)),
          split = "%",
          fixed = T
        ),
        recursive = F,
        use.names = F
      ))
  }
  # clean elements
  elements <- elements[!is.na(elements)]
  elements <- elements[elements != "NA"]

  if(all.possible){
    # make data frame with all possible combinations between elements
    element_table <-
      all_combinations(elem.bout)
  }

  # unlist element bout; for cases of overlapping elements, extract using random assignment of order
  elem.bout.unlisted <- unlist_list(elem.bout,
                                    method = "random")

  # create data frame that contains all sequences as 'sentences' separated by space
  elem.df <-
    data.frame(elem = unlist(
      map(elem.bout.unlisted, paste, collapse = " "),
      recursive = F,
      use.names = F
    ))

  # use tidytext functions to create bigrams
  elem.bi <- elem.df %>%
    unnest_tokens(bigram,
                  .data$elem,
                  n = 2,
                  token = "ngrams",
                  to_lower = FALSE
    ) %>%
    dt_separate(bigram, c("antecedent", "consequent"), sep = " ") %>%
    distinct() %>%
    filter(.data$antecedent %in% elements &
             .data$consequent %in% elements)

  # calculate total number of elements in the data that show any transition
  total.possible <- (
    elem.df %>%
      unnest_tokens(.data$word,
                    .data$elem,
                    to_lower = FALSE) %>%
      count(.data$word) %>%
      summarize(sum(.data$n))
  )[[1]]


  # add antecedent and consquent counts and probabilities
  elem.bi <-
    elem.bi %>%
    inner_join(
      elem.df %>%
        unnest_tokens(.data$antecedent,
                      .data$elem,
                      to_lower = FALSE) %>%
        count(.data$antecedent,
              name = "count.antecedent"),
      by = "antecedent"
    ) %>%
    inner_join(
      elem.df %>%
        unnest_tokens(.data$consequent,
                      .data$elem,
                      to_lower = FALSE) %>%
        count(.data$consequent,
              name = "count.consequent"),
      by = "consequent"
    ) %>%
    mutate(prob.antecedent = .data$count.antecedent / total.possible) %>%
    mutate(prob.consequent = .data$count.consequent / total.possible) %>%
    as_tibble() %>%
    suppressMessages()

  if(all.possible){
    transitions <- element_table %>%
      left_join(elem.bi) %>%
      replace_na(list(count.antecedent = 0,
                      count.consequent = 0,
                      prob.antecedent = 0,
                      prob.consequent = 0)) %>%
      arrange(.data$antecedent,
              .data$consequent) %>%
      suppressMessages()

    return(as_tibble(transitions))
  }
  # order
  transitions <-
    elem.bi %>%
    arrange(.data$antecedent,
            .data$consequent)

  return(as_tibble(transitions))
}
