#' Create matrix of element transitions, across possible random sequences for co-occurring elements
#'
#' @param elem.bout list of vectors of elements. Co-occurring elements are marked by the percentage symbol.
#' @param it how many iterations of the random splitting of co-occurring elements should be used? If elements never co-occur, this can be set to 1. If there are many co-occurrences, this number should be high, but this will make things slower
#' @param ran.method for co-occurring elements, how should they be split? Can be 'random' (order is randomised), or 'sample' (randomly choose any)
#'
#' @return Function returns a transition matrix. Because it sums over different co-occurrence splits, transitions numbers are not integers
#'
#' @importFrom dplyr mutate arrange desc as_tibble select all_of na_if inner_join left_join bind_rows bind_cols rename mutate_all distinct row_number summarize tibble filter count distinct
#' @importFrom stringr str_detect str_split str_replace_all str_to_title str_to_lower
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na gather separate unite
#' @importFrom tidytext unnest_tokens
#' @importFrom tidyfast dt_separate
#' @importFrom reshape2 acast
#' @importFrom rlang .data
#'
#' @author Alex Mielke
#' @export
#'
#' @examples
#' ###
elem_bout_matrix <- function(elem.bout,
                             it = 1,
                             ran.method = "random") {
  # unlist and clean element bout
  elem.bout <- elem.bout %>%
    lapply(str_replace_all,
           pattern = "-|/|_",
           replacement = "")
  # create vector with all unique elements
  elements <-
    unique(unlist(
      strsplit(as.character(unlist(elem.bout)), split = "%", fixed = T),
      recursive = F,
      use.names = F
    ))
  elements <- elements[!is.na(elements)]
  elements <- elements[elements != "NA"]

  # make matrix of all possible elements
  seq.matrix <-
    matrix(0, ncol = length(elements), nrow = length(elements))
  colnames(seq.matrix) <- rownames(seq.matrix) <- sort(elements)

  # create matrizes with transition counts for every iteration
  ran.matrixes <- lapply(1:it, function(j) {
    # assign sequence matrix
    seq.ran <- seq.matrix
    # unlist element bout
    elem.bout.unlisted <-
      unlist_list(elem.bout,
                  method = ran.method)

    # create data frame that contains all sequences as 'sentences' separated by space
    elem.df <-
      tibble(elem = unlist(
        lapply(elem.bout.unlisted,
               paste,
               collapse = " "),
        recursive = F,
        use.names = F
      ))

    # use tidytext functions to create bigrams
    elem.bi <- elem.df %>%
      unnest_tokens(
        bigram,
        elem,
        token = "ngrams",
        n = 2,
        to_lower = FALSE
      )

    return(elem.bi)
  })

  # add matrizes together
  seq.matrix <-
    bind_rows(ran.matrixes) %>%
    dt_separate(bigram,
                c("antecedent", "consequent"),
                sep = " ") %>%
    data.frame() %>%
    reshape2::acast(antecedent ~ consequent,
                    length,
                    margins = F) %>%
    data.frame() %>%
    suppressMessages()

  seq.matrix <- seq.matrix[rownames(seq.matrix) %in% elements,
                           colnames(seq.matrix) %in% elements]

  return(seq.matrix / it)
}
