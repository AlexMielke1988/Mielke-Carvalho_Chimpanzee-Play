#' Checks probabilities of combinations of 2 elements in the dataset based on a 'bag-of-words' approach: user either defines local context (elements within a time gap to the left or right) or select full bout
#'
#' @param elem.bout list of vectors of elements. Co-occurring elements are marked by the percentage symbol.
#' @param elem.time list of vectors of timings of elements. If set to NULL, the order of elements is used with distance of 1
#' @param gap vector with two values, one for the start of the bag (compared to element itself) and one with the end (e.g., c(-1,1) starts one second before the element and finishes one second after). Number of elements in this time frame can be variable.
#'
#' @return Function returns a data frame with the element combinations and how often one was in the bag of the other one
#'
#' @importFrom dplyr mutate arrange desc as_tibble select all_of na_if left_join bind_rows bind_cols rename mutate_all distinct row_number
#' @importFrom stringr str_detect str_split str_replace_all str_to_title str_to_lower
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na gather separate unite
#' @importFrom rlang .data
#' @importFrom stats aggregate na.omit
#'
#' @author Alex Mielke
#' @export
#'
#'
bag_of_words <-
  function(elem.bout,
           elem.time = NULL,
           gap = c(-1, 1)) {

    # select all possible combinations
    bag.list <- all_combinations(elem.bout = elem.bout)

    # created observed.sum column
    bag.list$observed.sum <- 0

    # if no elem.time is defined, use element number
    if (is.null(elem.time)) {
      elem.time <- lapply(elem.bout, seq_along)
    }


    # bootstrap expected number
    bout.sums <- lapply(1:length(elem.bout), function(z) {
      x <- elem.bout[[z]]
      x.times <- elem.time[[z]]

      ## doesn't need the shuffling approach to co-occurrence because all co-occurrences are included
      ran.matrixes <- lapply(1, function(j) {
        # make vector to store info
        mock.count <- rep(0, length(bag.list$antecedent))
        # unlist elements
        yy <- unlist_vector(x, method = "random")
        # unlist time vector as well
        y.times <- unlist(lapply(1:length(x), function(y) {
          if (is.na(x[y])) {
            return(x.times[y])
          }
          rep(x.times[y], times = sum(unlist(
            strsplit(as.character(x[y]), split = "", fixed = T), FALSE, FALSE
          ) == "%") + 1)
        }), FALSE, FALSE)

        # ignore everything for 2 or fewer elements
        if (length(yy[!is.na(yy)]) < 2) {
          return(NA)
        }
        if (length(yy[!is.na(yy)]) >= 2) {
          for (i in 1:length(yy)) {
            if (!is.na(yy[i])) {
              if (!"all" %in% gap) {
                items <-
                  which(y.times >= (y.times[i] + gap[1]) &
                    y.times <= (y.times[i] + gap[2]))
              }
              if ("all" %in% gap) {
                items <- 1:length(yy)
              }
              xy <-
                unique(yy[items[items > 0 & items < length(yy) & items != i]])
              xx <-
                bag.list[, 1] == yy[i] & bag.list[, 2] %in% xy[!is.na(xy)]
              mock.count[xx] <-
                mock.count[xx] + 1
            }
          }
        }
        return(mock.count)
      })
      mock.count <- rowSums(do.call(cbind, ran.matrixes))
      return(mock.count)
    })

    dyad.trans <- do.call(rbind, bout.sums)
    bag.list$observed.sum <- colSums(dyad.trans, na.rm = T)
    dyad.trans <- bag.list

    # how many possible matches does antecedent have?
    base.cond <-
      aggregate(dyad.trans$observed.sum, by = list(dyad.trans$antecedent), sum)
    count.antecedent <-
      base.cond$x[match(dyad.trans$antecedent, base.cond$Group.1)]

    # how often does descendant happen
    base.cond <-
      aggregate(dyad.trans$observed.sum, by = list(dyad.trans$consequent), sum)
    count.consequent <-
      base.cond$x[match(dyad.trans$consequent, base.cond$Group.1)]

    # conditional probability
    observed.probs <- unlist(dyad.trans$observed.sum / count.antecedent)

    mi <- (log(
      (dyad.trans$observed.sum / sum(dyad.trans$observed.sum)) /
        (
          count.antecedent / sum(dyad.trans$observed.sum) *
            count.consequent / sum(dyad.trans$observed.sum)
        )
    )) /
      (-1 * log(dyad.trans$observed.sum / sum(dyad.trans$observed.sum)))

    return(
      cbind(
        dyad.trans,
        count.antecedent = count.antecedent,
        observed.probs = observed.probs,
        mi = mi
      )
    )
  }
