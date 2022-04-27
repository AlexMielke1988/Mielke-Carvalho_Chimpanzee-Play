#' Checks probabilities of combinations of n elements in the dataset (e.g., n = 1: 'slap', n = 2: 'mount:slap', n = 3: 'mount:slap:scream')
#'
#' @param elem.bout list of vectors of elements. Co-occurring elements are marked by the percentage symbol.
#' @param lvl size of combinations of following elements. lvl = 0 (default) means the algorithm finds single elements; lvl = 1 takes dyadic combinations etc
#' @param it how many iterations of the random splitting of co-occurring elements should be used? If elements never co-occur, this can be set to 1. If there are many co-occurrences, this number should be high, but this will make things slower
#' @param ran.method for co-occurring elements, how should they be split? Can be 'random' (order is randomised), or 'sample' (randomly choose any)
#'
#' @return Function returns a transition matrix. Because it sums over different co-occurrence splits, transitions numbers are not integers
#'
#' @importFrom dplyr mutate arrange desc as_tibble select all_of na_if inner_join left_join bind_rows bind_cols rename mutate_all distinct row_number summarize tibble filter count distinct group_by ungroup n summarise right_join
#' @importFrom stringr str_detect str_split str_replace_all str_to_title str_to_lower
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na gather separate unite
#' @importFrom purrr map
#'
#' @author Alex Mielke
#' @export
#'
#' @examples
#' ###
element_combinations <- function(elem.bout,
                                 lvl = 0,
                                 it = 1,
                                 ran.method = "random") {
  # define level
  lvl.used = lvl
  if(lvl == 0){lvl.used = 1}
  # add NAs at the end of every sequence
  elem.bout.combs <- lapply(elem.bout, function(x) {
    c(x, rep(NA, lvl.used))
  })

  # create vector with all unique elements
  elements <-
    sort(unique(unlist(
      strsplit(as.character(unlist(elem.bout.combs, FALSE, FALSE)), split = "%", fixed = T),
      FALSE, FALSE
    )))

  # unlist element bouts
  elem.bout.combs <- as.vector(unlist(elem.bout.combs, FALSE, FALSE))
  elem.bout.combs <- unlist_vector(elem.bout.combs,
                                   method = ran.method)


  # assign each element the number of the vector they occur in
  elem.nr <- unlist(lapply(1:length(elem.bout.combs), function(x) {
    rep(x, length(unlist_vector(elem.bout.combs[[x]],
                                method = ran.method
    )))
  }))

  # create list of antecedents and consquents, and the bout they belong to
  elem.combs <- purrr::map(
    seq(1, length(elem.bout.combs) - lvl.used - 1, by = 1),
    ~ list(
      antecedent = elem.bout.combs[.x:(.x + lvl.used - 1)],
      consequent = elem.bout.combs[.x + lvl.used],
      bout.nr = elem.nr[.x + lvl.used]
    )
  )
  elem.combs <- purrr::transpose(elem.combs)

  # remove those where the antecedent or consequent is NA
  rem <- sapply(elem.combs$antecedent, function(x) {
    mean(!is.na(x)) != 1
  }) |
    sapply(elem.combs$consequent, function(x) {
      mean(!is.na(x)) != 1
    })
  elem.combs$antecedent <- elem.combs$antecedent[!rem]
  elem.combs$consequent <- elem.combs$consequent[!rem]
  elem.nr <- elem.combs$bout.nr[!rem]

  # if the antecedent contains multiple elements, combine them
  if(lvl > 1){elem.combs$antecedent = sapply(elem.combs$antecedent,
                                             paste,
                                             collapse = '%')}

  # unlist antecedents and consequents
  elem.combs$antecedent <- unlist(elem.combs$antecedent,
                                  use.names = FALSE
  )
  elem.combs$consequent <- unlist(elem.combs$consequent,
                                  use.names = FALSE
  )

  # count antecedent and consequent combinations
  all.combs <- cbind(antecedent = elem.combs$antecedent,
                     consequent = elem.combs$consequent) %>%
    data.frame %>%
    group_by(.data$antecedent,
             .data$consequent) %>%
    summarize(Freq = n()) %>%
    ungroup() %>%
    suppressMessages()

  # if lvl == 0, just count antecedents
  if(lvl == 0){
    all.combs <- all.combs %>%
      mutate(count = .data$Freq) %>%
      group_by(.data$antecedent) %>%
      summarise(count = sum(.data$count)) %>%
      ungroup() %>%
      mutate(
        combination = .data$antecedent,
        consequent = '',
        probability.total = .data$count/sum(.data$count),
        probability.transitions = .data$count/sum(.data$count))
  }
  # if lvl > 0, create combination and count it
  if(lvl > 0){
    total.n <- sum(all.combs$Freq)
    all.combs <- all.combs %>%
      filter(!is.na(.data$consequent)) %>%
      unite(combination,
            .data$antecedent,
            .data$consequent,
            sep = '%',
            remove = FALSE) %>%
      mutate(count = .data$Freq) %>%
      select(-.data$Freq)

    all.combs <- all.combs %>%
      group_by(.data$antecedent) %>%
      summarise(ant.count = sum(.data$count)) %>%
      ungroup() %>%
      right_join(all.combs) %>%
      mutate(probability.total = .data$count/total.n,
             probability.transitions = .data$count/.data$ant.count) %>%
      select(-.data$ant.count) %>%
      suppressMessages()
  }

  # sort it all
  all.combs$combination <- as.character(all.combs$combination)
  all.combs <- all.combs %>%
    arrange(.data$antecedent, .data$consequent)

  return(all.combs)
}
