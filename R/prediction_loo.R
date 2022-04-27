#' Tests accuracy of markov transition in predicting next elements by creating transition probabilities for all bouts bar one, and then trying to predict that bout
#'
#' @param elem.bout list of vectors of elements. Co-occurring elements are marked by the percentage symbol.
#' @param elem.time list of vectors of timings of elements. If set to NULL, the order of elements is used with distance of 1
#' @param it how many iterations of the random splitting of co-occurring elements should be used? If elements never co-occur, this can be set to 1. If there are many co-occurrences, this number should be high, but this will make things slower
#' @param lvl number representing the order/antecedent length at which predictions should be made. 0 is base probability of consequent, 1 is transition given 1 antecedent (e.g., 'Hit'), 2 is bigram antecedent (e.g., 'Hit:Slap') etc
#' @param prediction how should prediction be made? If 'PPM' (Prediction by partial matching), prediction is made based on the highest order for which information is available; if 'product', prediction is made based on the product of all orders.
#' @param trials number of iterations of assignments
#' @param cores number of cores for parallel randomization (default is 2)
#' @param gap number of elements to each side that should be counted in the same 'bag' as the element; if 'all', use whole bout
#' @param out number of elements left out per leave-one-out iteration
#' @param ran.method for co-occurring elements, how should they be split? Can be 'random' (order is randomised), or 'sample' (randomly choose any)
#'
#' @return Returns the mean value of correct classifications
#'
#' @importFrom parallel parLapply stopCluster mclapply makeCluster detectCores clusterExport clusterCall
#' @importFrom doParallel registerDoParallel
#' @importFrom purrr map transpose
#' @importFrom ggplot2 ggplot geom_label aes ggtitle xlab ylab theme_classic annotate geom_hline coord_fixed scale_size_continuous arrow unit scale_size theme_minimal theme element_text scale_fill_gradient2 geom_tile
#' @importFrom dplyr mutate arrange desc as_tibble select all_of na_if left_join bind_rows bind_cols rename mutate_all distinct row_number
#' @importFrom stringr str_detect str_split str_replace_all str_to_title str_to_lower
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na gather separate unite
#' @importFrom igraph get.adjacency
#'
#'
#' @author Alex Mielke
#' @export
#'
#' @examples
#' ###
prediction_loo <- function(elem.bout,
                           it = 1,
                           trials = 100,
                           cores = 10,
                           lvl = 1,
                           gap = NULL,
                           elem.time = NULL,
                           prediction = 'PPM',
                           out = 10,
                           ran.method = "random") {
  # cut training and test samples into same sized samples
  training_index.i <- cut(seq_along(elem.bout), out)
  xx <- sample(seq_along(elem.bout))
  training_index.i <- lapply(unique(training_index.i), function(x) {
    xx[which(training_index.i == x)]
  })

  # clean up elem.bout
  elem.bout <- elem.bout %>%
    lapply(str_replace_all,
      pattern = "-|/|_",
      replacement = ""
    )
  # add NA at end of each sequence, depending on number of lvl
  elem.bout <- lapply(elem.bout, function(x) {
    c(x, rep(NA, lvl))
  })

  # make sorted list of all elements
  elements <- elem.bout %>%
    unlist(FALSE, FALSE) %>%
    strsplit(split = "%", fixed = T) %>%
    unlist(FALSE, FALSE) %>%
    unique()
  elements <- elements[!is.na(elements) & !(elements == "NA")]

  #go parallel
  mycluster <- makeCluster(cores, type = "PSOCK")
  # export the relevant information to each core
  clusterExport(
    cl = mycluster,
    c(
      "elem.bout",
      "elem.time",
      "elements",
      "unlist_list",
      "unlist_vector",
      "elem_bout_matrix",
      "gap",
      "bag_of_words",
      "lvl",
      "ran.method",
      "element_combinations",
      "it",
      "transpose",
      "map",
      "trials",
      "prediction"
    ),
    envir = environment()
  )
  registerDoParallel(mycluster)
  clusterCall(mycluster, function() {
    library(dplyr)
  })
  clusterCall(mycluster, function() {
    library(stringr)
  })
  clusterCall(mycluster, function() {
    library(tidyr)
  })
  clusterCall(mycluster, function() {
    library(tidytext)
  })
  clusterCall(mycluster, function() {
    library(tidyfast)
  })
  # run parallel loop

  # go through every index, calculate probabilities for training data (all other bouts), and try to predict within bouts
  loo.pred <-
    parLapply(mycluster, X = training_index.i, function(training_index) {
      # if no gap is defined, use transition probabilities
      if (is.null(gap)) {
        # define training and test data
        boot.train <- elem.bout[-training_index]
        boot.test <- elem.bout[training_index]

        # calculate probabilities for training data
        # for 0-order (ie each element by itself)
        llh.0 <- element_combinations(
          elem.bout = boot.train,
          lvl = 0,
          it = 1,
          ran.method = ran.method
        )

        if (lvl == 0) {
          llh <- llh.0
        }
        # if higher level, calculate for all lower levels
        if (lvl > 0) {
          llh <- lapply(1:lvl, function(x) {
            element_combinations(
              elem.bout = boot.train,
              lvl = x,
              it = it,
              ran.method = ran.method
            )
          })
          names(llh) <- 1:lvl
        }
      }
      # if gap is defined, use bag-of-words approach
      if (!is.null(gap)) {
        # define training and test datasets, replace 'NA' with actual NAs
        boot.train <- elem.bout[-training_index]
        boot.train <- lapply(boot.train, function(z) {
          z <- ifelse(z == "NA", NA, z)
        })
        boot.test <- elem.bout[training_index]
        boot.test <- lapply(boot.test, function(z) {
          z <- ifelse(z == "NA", NA, z)
        })

        # if no elem.time is defined, each element gets its number in sequence
        if (is.null(elem.time)) {
          time.train <- lapply(1:length(boot.train), function(z) {
            1:length(boot.train[[z]])
          })
        }
        # if time is defined, assign it to training data
        if (!is.null(elem.time)) {
          time.train <- elem.time[-training_index]
        }

        # calculate bag of words transitions
        llh <- bag_of_words(
          elem.bout = boot.train,
          elem.time = time.train,
          gap = gap
        )
        # for probabilities that are NA, set 0
        llh$observed.probs <-
          ifelse(
            is.na(llh$observed.probs),
            0,
            llh$observed.probs
          )
        llh$count <- llh$observed.sum
        llh$probability.transitions <- llh$observed.probs
      }
      # create random matrices
      ran.matrixes <- lapply(1:trials, function(j) {
        # unlist test data
        boot.test <- as.vector(unlist(boot.test))
        boot.test <- unlist_vector(boot.test, method = ran.method)
        boot.test <- ifelse(boot.test == "NA", NA, boot.test)

        # if the test data is shorter than the selected level, return NA
        if (length(boot.test) - lvl < 1) {
          return(NA)
        }
        # create list that splits antecedents and consequents for each event
        dataset.test <- purrr::map(
          seq(1, length(boot.test) - lvl, by = 1),
          ~ list(antecedent = boot.test[.x:(.x + lvl - 1)], consequent = boot.test[.x + lvl])
        )
        dataset.test <- purrr::transpose(dataset.test)

        # remove cases where either the antecedent or consequent contains NAs
        rem <- sapply(dataset.test$antecedent, function(x) {
          mean(!is.na(x)) != 1
        }) | sapply(dataset.test$consequent, function(x) {
          mean(!is.na(x)) != 1
        })
        dataset.test$antecedent <- dataset.test$antecedent[!rem]
        dataset.test$consequent <- dataset.test$consequent[!rem]

        # if afterwards no data are left, return NA
        if (is.null(unlist(dataset.test$antecedent)) |
          is.null(unlist(dataset.test$consequent))) {
          return(NA)
        }
        # if the order of interest is higher than 0, determine transition probabilities for all antecedents
        if (lvl > 0) {
          # go order by order
          ants.probs <- lapply(1:lvl, function(x) {
            # split antecedents
            ants <-
              sapply(llh[[x]]$antecedent,
                strsplit,
                split = "%",
                fixed = T
              )
            conse <-
              sapply(llh[[x]]$consequent,
                strsplit,
                split = "%",
                fixed = T
              )
            probs <- llh[[x]]$probability.transitions
            return(list(
              antecedent = ants,
              consequent = conse,
              probability = probs
            ))
          })

          # transpose list
          ants.probs <- purrr::transpose(ants.probs)
          # unlist list elements
          ants.probs$antecedent <-
            unlist(ants.probs$antecedent,
              recursive = FALSE,
              use.names = FALSE
            )
          ants.probs$consequent <-
            unlist(ants.probs$consequent,
              recursive = FALSE,
              use.names = FALSE
            )
          ants.probs$probability <-
            unlist(ants.probs$probability,
              recursive = FALSE,
              use.names = FALSE
            )
        }
        # go through each event of interest, make predictions based on the antecedents and their transition probabilities
        pred <-
          lapply(1:length(dataset.test$antecedent), function(x) {
            if (lvl > 0) {
              ### what is the antecedent depending on the level?
              x.ants <- lapply(1:lvl, function(y) {
                rev(rev(unlist(dataset.test$antecedent[x]))[1:y])
              })
              ### where are those antecedents in the ants.probs list?
              x.llhs <- lapply(1:lvl, function(z) {
                as.vector(which(sapply(ants.probs$antecedent, function(y) {
                  identical(x.ants[[z]], y)
                })))
              })
              ### which elements are in the positions in question?
              x.element <- lapply(1:lvl, function(z) {
                as.vector(unlist(ants.probs$consequent[x.llhs[[z]]]))
              })
              ### what are the  transition probabilities of those elements
              x.chosen <- lapply(1:lvl, function(z) {
                as.vector(unlist(ants.probs$probability[x.llhs[[z]]]))
              })
              ### are any levels empty, i.e. no cases occur in the training data?
              non.empty <- sapply(x.element, function(z) {
                length(z) > 0
              })
              x.ants <- x.ants[non.empty]
              x.llhs <- x.llhs[non.empty]
              x.element <- x.element[non.empty]
              x.chosen <- x.chosen[non.empty]

              ### establish baseline probability of each element in 0-order
              all.consequents <- unique(unlist(llh[[1]]$consequent))
              all.consequent.probs <-
                data.frame(antecedent = all.consequents) %>%
                left_join(llh.0) %>%
                select(.data$probability.total) %>%
                unlist(FALSE, FALSE) %>%
                suppressMessages()

              ### combine 0-order with all other orders
              x.element <-
                c(list(as.vector(all.consequents)), x.element)
              x.chosen <-
                c(list(as.vector(all.consequent.probs)), x.chosen)

              ### make sure that choices follow same order as elements
              x.choices <- lapply(1:length(x.element), function(z) {
                x.chosen[[z]][match(x.element[[1]], x.element[[z]])]
              })
              ### if choice was never observed (given NA), give it the minimum observed value
              x.choices <- lapply(x.choices, function(z) {
                ifelse(is.na(z), min(unlist(x.choices), na.rm = T), z)
              })
              # ### Weight higher order higher, because it is more rare
              # x.choices <- lapply(seq_along(x.choices), function(z) {
              #   z * x.choices[[z]]
              # })
              # if(length(x.choices) > 1){x.choices[[1]] = NULL}
              # combine the probabilities
              if(prediction == 'PPM'){x.chosen = x.choices[length(x.choices)] %>%
                unlist(F,F)}
              if(prediction == 'product'){x.chosen <- apply(do.call(cbind, x.choices), 1, prod)}
              x.element <- unlist(x.element[1])
              x.chosen <- x.chosen / sum(x.chosen)
            }

            # if lvl ==0, just use the descendant probabilities
            if (lvl == 0) {
              xx <- table(unlist(strsplit(unlist(
                elem.bout
              ), split = "%"), FALSE, FALSE))
              x.element <- names(xx)
              x.chosen <- as.numeric(xx)
              x.chosen <- x.chosen / sum(x.chosen)
            }
            # sample to make predictions
            # one element
            simple.sample <- sample(x.element, 1, prob = x.chosen)
            # three elements
            triple.sample <- sample(x.element, 3, prob = x.chosen)

            return(list(simple.sample = simple.sample, triple.sample = triple.sample))
          })
        # transpose prediction list
        pred <- purrr::transpose(pred)
        # check whether predictions were correct
        outcome.simple <-
          unlist(pred$simple.sample) == unlist(dataset.test$consequent)
        # check whether prediction was in top three
        outcome.triple <-
          sapply(seq_along(pred$triple.sample), function(x) {
            dataset.test$consequent[x] %in% pred$triple.sample[[x]]
          })
        out.simple <-
          data.frame(unlist(dataset.test$consequent), outcome.simple)
        out.triple <-
          data.frame(unlist(dataset.test$consequent), outcome.triple)

        return(
          list(
            mean.res.single = mean(outcome.simple, na.rm = T),
            mean.res.three = mean(outcome.triple, na.rm = T),
            sample.size = unlist(dataset.test$consequent) %>% length(),
            expected = unlist(dataset.test$consequent),
            observed = pred$simple.sample
          )
        )
      })

      # transpose all trials
      ran.matrixes <- transpose(ran.matrixes)
      # if nothing happened, return NAs
      if (length(ran.matrixes) == 1) {
        return(
          list(
            mean.res.single = NA,
            mean.res.three = NA,
            sample.size = NA,
            expected = NA,
            observed = NA
          )
        )
      }
      # mean correct predictions
      mean.res.single <-
        mean(unlist(ran.matrixes$mean.res.single), na.rm = T)
      # mean correct predictions in top three
      mean.res.three <-
        mean(unlist(ran.matrixes$mean.res.three), na.rm = T)
      #mean sample size
      sample.size <- mean(unlist(ran.matrixes$sample.size), na.rm = T)
      # preserve observed and expected values
      observed <- unlist(ran.matrixes$observed)
      expected <- unlist(ran.matrixes$expected)

      return(
        list(
          accuracy = mean.res.single,
          accuracy.top.three = mean.res.three,
          sample.size = sample.size,
          observed = observed,
          expected = expected
        )
      )
    })
  stopCluster(mycluster)

  # transpose lists
  loo_nn <- transpose(loo.pred)


  # Evaluation Model Choice -------------------------------------------------

  # select all elements as data frame and how often they were expected
  results <- data.frame(t(table(unlist(loo_nn$expected))))[, 2:3]
  colnames(results) <- c("element", "expected")
  # account for number of trials
  results$expected <- round(results$expected / trials, 3)
  # define observed
  obs <- t(table(unlist(loo_nn$observed)))[1, ] / trials
  results$observed <-
    round(obs[match(results$element, names(obs))], 3)
  # define correct
  correct <-
    t(table(unlist(loo_nn$observed)[unlist(loo_nn$observed) == unlist(loo_nn$expected)]))[1, ] / trials
  results$correct <- correct[match(results$element, names(correct))]

  # set NA as 0s
  results$observed <-
    ifelse(is.na(results$observed), 0, results$observed)
  results$correct <-
    ifelse(is.na(results$correct), 0, results$correct)
  results$true.positive <-
    round(results$correct / results$expected, 3)

  # determine number of false positions
  false.pos <-
    t(table(unlist(loo_nn$observed)[unlist(loo_nn$observed) != unlist(loo_nn$expected)]))[1, ] / trials
  false.positive <-
    false.pos[match(results$element, names(false.pos))]
  false.positive <- ifelse(is.na(false.positive), 0, false.positive)
  results$false.positive <-
    round(false.positive / results$observed, 3)

  # define number of false negatives
  false.neg <-
    t(table(unlist(loo_nn$expected)[unlist(loo_nn$observed) != unlist(loo_nn$expected)]))[1, ] / trials
  false.negative <-
    false.neg[match(results$element, names(false.neg))]
  false.negative <- ifelse(is.na(false.negative), 0, false.negative)
  results$false.negative <-
    round(false.negative / results$expected, 3)


  # Misclassification Matrix ------------------------------------------------

  # make misclassification plot
  misclass <- data.frame(t(table(
    unlist(loo_nn$expected),
    unlist(loo_nn$observed)
  )))
  colnames(misclass) <- c("observed", "expected", "count")
  tot.exp <- misclass %>%
    group_by(.data$expected) %>%
    summarise(tot.exp = sum(.data$count))

  misclass <- misclass %>%
    left_join(tot.exp) %>%
    mutate(misclassification.probability = round(.data$count / tot.exp, 3)) %>%
    select(-.data$tot.exp) %>%
    suppressMessages()

  mis.matr <- graph_from_edgelist(as.matrix(misclass[, c("expected", "observed")]),
    directed = T
  )
  edge.attributes(mis.matr)$probability <- misclass$misclassification.probability
  mis.matr <-
    mis.matr %>%
    get.adjacency(attr = "probability") %>%
    as.matrix()

  mis.plot <- ggplot(
    data = misclass,
    aes(x = .data$expected, y = .data$observed, fill = .data$misclassification.probability)
  ) +
    geom_tile() +
    scale_fill_gradient2(
      low = "white",
      high = "red",
      limit = c(0, max(
        misclass$misclassification.probability
      )),
      space = "Lab",
      name = "Confusion Probability"
    ) +
    theme_minimal() +
    xlab("expected") +
    ylab("observed") +
    theme(axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      size = 10,
      hjust = 1
    )) +
    ggtitle(paste(c(
      "Misclassification Heat Map, Level = ", lvl
    ), collapse = " "))


  return(
    list(
      accuracy = mean(unlist(loo_nn$accuracy), na.rm = T),
      accuracy.top.three = mean(unlist(loo_nn$accuracy.top.three), na.rm = T),
      sample.size = mean(unlist(loo_nn$sample.size), na.rm = T),
      element.results = results,
      confusion.matrix = mis.matr,
      heatmap = mis.plot
    )
  )
}
