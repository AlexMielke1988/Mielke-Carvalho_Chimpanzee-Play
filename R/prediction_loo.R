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
#' @importFrom dplyr mutate arrange desc as_tibble select all_of na_if left_join bind_rows bind_cols rename mutate_all distinct row_number mutate_if
#' @importFrom stringr str_detect str_split str_replace_all str_to_title str_to_lower
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na gather separate unite
#' @importFrom igraph get.adjacency
#' @importFrom e1071 naiveBayes
#' @importFrom sbo sbo_predictor
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
                           prediction = "PPM",
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

  # go parallel
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
  clusterCall(mycluster, function() {
    library(e1071)
  })
  clusterCall(mycluster, function() {
    library(sbo)
  })
  
  # run parallel loop

  # go through every index, calculate probabilities for training data (all other bouts), and try to predict within bouts
  loo.pred <-
    parLapply(mycluster,
      X = training_index.i,
      function(training_index) {
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
            ifelse(is.na(llh$observed.probs),
              0,
              llh$observed.probs
            )
          llh$count <- llh$observed.sum
          llh$probability.transitions <- llh$observed.probs
        }



# Start Repeated Trials ---------------------------------------------------

        
        # create random matrices
        ran.matrixes <- lapply(1:trials, function(j) {
          # unlist test data
          boot.test <- as.vector(unlist(boot.test))
          boot.test <-
            unlist_vector(boot.test, method = ran.method)
          boot.test <-
            ifelse(boot.test == "NA", NA, boot.test)

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
          rem <-
            sapply(dataset.test$antecedent, function(x) {
              mean(!is.na(x)) != 1
            }) |
            sapply(dataset.test$consequent, function(x) {
              mean(!is.na(x)) != 1
            })
          dataset.test$antecedent <-
            dataset.test$antecedent[!rem]
          dataset.test$consequent <-
            dataset.test$consequent[!rem]
          
          # if afterwards no data are left, return NA
          if (is.null(unlist(dataset.test$antecedent)) |
              is.null(unlist(dataset.test$consequent))) {
            return(NA)
          }
          
          # prepare data for Naive Bayes classifier
          
          
          # Naive Bayes Classifier --------------------------------------------------
          
          
          # make test and training sets
          train_sets <- lapply(1:lvl, function(k) {
            elem.bout.train <- as.vector(unlist(elem.bout))
            elem.bout.train <-
              unlist_vector(elem.bout.train)
            # make sure you keep element number so each antecedent can be associated with the training index
            elem.nr <-
              unlist(lapply(1:length(elem.bout), function(x) {
                rep(x, length(unlist_vector(elem.bout[[x]])))
              }))
            elem.bout.train <- as.character(elem.bout.train)
            # create training set for Naive Bayes
            
            dataset.train <- purrr::map(
              seq(1, length(elem.bout.train) - k - 1, by = 1),
              ~ list(
                antecedent = elem.bout.train[.x:(.x + k - 1)],
                consequent = elem.bout.train[.x + k],
                bout.nr = elem.nr[.x + k]
              )
            )
            
            dataset.train <- purrr::transpose(dataset.train)
            
            rem <-
              sapply(dataset.train$antecedent, function(x) {
                mean(!is.na(x)) != 1
              }) |
              sapply(dataset.train$consequent, function(x) {
                mean(!is.na(x)) != 1
              })
            dataset.train$antecedent <-
              dataset.train$antecedent[!rem]
            dataset.train$consequent <-
              dataset.train$consequent[!rem]
            elem.nr <- dataset.train$bout.nr[!rem]
            
            # combinations <- lapply(dataset.train$antecedent, function(au){
            #   xx = unlist(au)
            #   lapply((length(xx)):1, function(ku){
            #     paste(xx[ku:length(xx)], collapse = '_')
            #   })
            # })
            #
            # combinations <- purrr::transpose(combinations)
            # combinations <- lapply(combinations, unlist)
            
            # dataset.train$antecedent <-
            #   do.call(cbind, combinations) %>%
            #   as.matrix()
            
            dataset.train$antecedent <-
              do.call(rbind, dataset.train$antecedent) %>%
              as.matrix()
            
            dataset.train$consequent <-
              unlist(dataset.train$consequent)
            
            dataset.test <- dataset.train
            dataset.test$antecedent <-
              dataset.test$antecedent[elem.nr %in% training_index, ]
            dataset.test$consequent <-
              dataset.test$consequent[elem.nr %in% training_index]
            
            train.set <-
              data.frame(cbind(
                desc = dataset.train$consequent,
                dataset.train$antecedent
              )) %>%
              mutate_if(is.character, as.factor)
            
            test.set <-
              data.frame(cbind(
                desc = dataset.test$consequent,
                dataset.test$antecedent
              )) %>%
              mutate_if(is.character, as.factor)
            
            return(list(
              train.set = train.set,
              test.set = test.set
            ))
          })
          
          
          # run naiveBayes algorithm from the e1071 package
          model.nb <- lapply(1:(lvl + 1), function(y) {
            y.minus <- y - 1
            if (y.minus == 0) {
              xx <-
                e1071::naiveBayes(desc ~ rep("o", nrow(train_sets[[y]]$train.set)),
                                  data = train_sets[[y]]$train.set,
                                  laplace = 0.01
                )
              return(xx)
            }
            if (y.minus > 0) {
              e1071::naiveBayes(desc ~ .,
                                data = train_sets[[y.minus]]$train.set,
                                laplace = 0.01
              )
            }
          })
          
          # predict test data based on model
          nb.prediction <- lapply(1:(lvl + 1), function(y) {
            y.minus <- y - 1
            if (y.minus == 0) {
              return(predict(model.nb[[y]], rep(
                "o", length(train_sets[[y]]$test.set[, 1])
              )))
            }
            if (y.minus == 1) {
              return(predict(model.nb[[y]], data.frame(V2 = train_sets[[y.minus]]$test.set[, -1])))
            }
            if (y.minus > 1) {
              return(predict(model.nb[[y]], data.frame(train_sets[[y.minus]]$test.set[, -1])))
            }
          })
          
          # store correct classifications
          nb.correct <- lapply(1:(lvl + 1), function(y) {
            y.minus <- y - 1
            if (y.minus == 0) {
              return(as.character(nb.prediction[[y]]) == as.character(train_sets[[y]]$test.set[, 1]))
            }
            if (y.minus > 0) {
              return(nb.prediction[[y]] == as.character(train_sets[[y.minus]]$test.set[, 1]))
            }
          })
          
          
          
          # Random Forest -----------------------------------------------------------
          # run random forest algorithm from the ranger package
          model.rf <- lapply(1:(lvl + 1), function(y) {
            y.minus <- y - 1
            if (y.minus == 0) {
              xx <-
                ranger::ranger(desc ~ .,
                               data = train_sets[[y]]$train.set %>% 
                                 mutate(V2 = 'o'),
                               mtry =  1, 
                               num.trees = 1000, 
                               importance = 'impurity'
                )
              return(xx)
            }
            if (y.minus > 0) {
              ranger::ranger(desc ~ .,
                             data = train_sets[[y.minus]]$train.set,
                             mtry =  (ncol(train_sets[[y.minus]]$train.set)-1), 
                             num.trees = 1000, 
                             importance = 'impurity'
              )
            }
          })
          
          # predict test data based on model
          rf.prediction <- lapply(1:(lvl + 1), function(y) {
            y.minus <- y - 1
            if (y.minus == 0) {
              return(predict(model.rf[[y]], data.frame(V2 = train_sets[[y]]$test.set[, -1]))$predictions)
            }
            if (y.minus == 1) {
              return(predict(model.rf[[y]], data.frame(V2 = train_sets[[y.minus]]$test.set[, -1]))$predictions)
            }
            if (y.minus > 1) {
              return(predict(model.rf[[y]], data.frame(train_sets[[y.minus]]$test.set[, -1]))$predictions)
            }
          })
          
          # store correct classifications
          rf.correct <- lapply(1:(lvl + 1), function(y) {
            y.minus <- y - 1
            if (y.minus == 0) {
              return(as.character(rf.prediction[[y]]) == as.character(train_sets[[y]]$test.set[, 1]))
            }
            if (y.minus > 0) {
              return(rf.prediction[[y]] == as.character(train_sets[[y.minus]]$test.set[, 1]))
            }
          })
          
          
          
          
          # SBO N-Gram classifier ---------------------------------------------------
          
          # turn bouts into 'sentences'
          elem.bout.train.k <- boot.train %>%
            unlist_list(method = 'random') %>% 
            lapply(str_replace_all,
                   pattern = "-|/|_",
                   replacement = ""
            ) %>% 
            lapply(function(x){
              f = unlist(x) 
              f[is.na(f)] = '.'
              return(f)
            }) %>% 
            lapply(function(x) {
              c(x, '.')
            })
          # concatenate all bouts, separated by .
          kgram_bouts <- lapply(elem.bout.train.k, 
                                function(x){
                                  paste(unlist(x), collapse = ' ')})
          kgram_bouts <- kgram_bouts %>% str_c()
          
          # make predictor using the sbo package
          sbo_pr <- sbo_predictor(object = kgram_bouts, # preloaded example dataset
                                  N = lvl, # Train a 3-gram model
                                  dict = target ~ 1, # cover 75% of training corpus
                                  .preprocess = identity, # Preprocessing transformation 
                                  EOS = ".?!:;", # End-Of-Sentence tokens
                                  lambda = 0.2, # Back-off penalization in SBO algorithm
                                  L = 1, # Number of predictions for input
                                  filtered = '<EOS>' # Exclude the <UNK> token from predictions
          )
          
          sbo_results <- lapply(1:(lvl + 1), function(y) {
            y.minus <- y - 1
            
            if(y.minus == 0){
              te.set <- train_sets[[y]]$test.set %>% select(-desc)
              te.set <- sapply(1:nrow(te.set), function(k) paste(unlist(te.set[k,]), collapse = ' '))
              
              prediction <- sapply(te.set, function(k) predict(sbo_pr, input = '')) %>% unlist(F, F)
              correct <- prediction == train_sets[[y]]$test.set$desc
            }
            if(y.minus > 0){
              te.set <- train_sets[[y.minus]]$test.set %>% select(-desc)
              te.set <- sapply(1:nrow(te.set), function(k) paste(unlist(te.set[k,]), collapse = ' '))
              
              prediction <- sapply(te.set, function(k) predict(sbo_pr, input = k)) %>% unlist(F, F)
              correct <- prediction == train_sets[[y.minus]]$test.set$desc
            }
            return(
              list(prediction = prediction,
                   correct = correct)
            )
          })
          names(sbo_results) <- 0:lvl
          sbo_results <- purrr::transpose(sbo_results)
          
          # N-Gram Interpolation classifier -----------------------------------------
          
          
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
          
          ### establish baseline probability of each element in 0-order
          xx.0 <-
            table(unlist(strsplit(unlist(elem.bout), split = "%"), FALSE, FALSE))
          x.element.0 <- names(xx.0)
          x.chosen.0 <- as.numeric(xx.0)
          x.chosen.0 <- x.chosen.0 / sum(x.chosen.0)
          
          all.consequents <-
            unique(unlist(x.element.0))
          all.consequent.probs <-
            data.frame(antecedent = all.consequents) %>%
            left_join(llh.0) %>%
            select(.data$probability.total) %>%
            unlist(FALSE, FALSE) %>%
            suppressMessages()
          
          # go through each event of interest, make predictions based on the antecedents and their transition probabilities
          pred <-
            lapply(1:length(dataset.test$antecedent), function(x) {
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
              # ### are any levels empty, i.e. no cases occur in the training data?
              # non.empty <- sapply(x.element, function(z) {
              #   length(z) > 0
              # })
              # x.ants <- x.ants[non.empty]
              # x.llhs <- x.llhs[non.empty]
              # x.element <- x.element[non.empty]
              # x.chosen <- x.chosen[non.empty]
              
              ### combine 0-order with all other orders
              x.element <-
                c(list(as.vector(all.consequents)), x.element)
              x.chosen <-
                c(list(as.vector(all.consequent.probs)), x.chosen)
              
              ### make sure that choices follow same order as elements
              x.choices <-
                lapply(1:length(x.element), function(z) {
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
              
              x.choices <- x.choices[-1]
              
              # combine the probabilities
              if (prediction == "PPM") {
                x.chosen <- lapply(1:lvl, function(y) {
                  xx <-
                    x.choices[y] %>%
                    unlist(F, F)
                  return(xx / sum(xx))
                })
              }
              if (prediction == "product") {
                x.chosen <- lapply(1:lvl, function(y) {
                  xx <- apply(do.call(cbind, x.choices[1:y]), 1, prod)
                  return(xx / sum(xx))
                })
              }
              x.element <- unlist(x.element[1])
              
              # for lvl ==0, just use the consequent probabilities
              
              x.chosen <- c(list(x.chosen.0), x.chosen)
              
              # sample to make predictions
              # one element
              simple.sample <-
                lapply(x.chosen, function(y) {
                  sample(x.element, 1, prob = y)
                })
              names(simple.sample) <- 0:lvl
              # three elements
              triple.sample <-
                lapply(x.chosen, function(y) {
                  sample(x.element, 3, prob = y)
                })
              names(triple.sample) <- 0:lvl
              
              # store probabilities
              simple.probs <-
                lapply(seq_along(x.chosen), function(y) {
                  x.chosen[[y]][which(x.element == dataset.test$consequent[x])]
                })
              names(simple.probs) <- 0:lvl
              
              return(
                list(
                  simple.sample = simple.sample,
                  triple.sample = triple.sample,
                  simple.probs = simple.probs
                )
              )
            })
          # transpose prediction list
          # pred <- purrr::transpose(pred)
          # check whether predictions were correct
          outcome.simple <-
            lapply(seq_along(pred), function(y) {
              lapply(pred[[y]]$simple.sample, function(k) {
                unlist(k) == dataset.test$consequent[y]
              })
            }) %>%
            purrr::transpose()
          # check whether prediction was in top three
          outcome.triple <-
            lapply(seq_along(pred), function(y) {
              lapply(pred[[y]]$triple.sample, function(k) {
                dataset.test$consequent[y] %in% k
              })
            }) %>%
            purrr::transpose()
          
          # save probabilities of true value
          probs.expected <-
            lapply(seq_along(pred), function(y) {
              lapply(pred[[y]]$simple.probs, function(k) {
                unlist(k)
              })
            }) %>%
            purrr::transpose() %>%
            lapply(unlist)
          
          all_out <-
            lapply(seq_along(outcome.simple), function(y) {
              y.minus <- y - 1
              if (y.minus == 0) {
                return(
                  list(
                    # N Gram
                    mean.res.single = mean(outcome.simple[[y]] %>% unlist(), na.rm = T),
                    mean.res.three = mean(outcome.triple[[y]] %>% unlist(), na.rm = T),
                    sample.size = unlist(dataset.test$consequent) %>% length(),
                    expected = unlist(dataset.test$consequent),
                    observed = sapply(purrr::transpose(pred)$simple.sample, function(k) {
                      k[[y]]
                    }),
                    # Naive Bayes
                    probs.expected = probs.expected[[y]],
                    nb.prediction = nb.prediction[[y]],
                    nb.expected = as.character(train_sets[[y]]$test.set[, 1]),
                    nb.correct = nb.correct[[y]],
                    # SBO
                    sbo.prediction = sbo_results$prediction[[y]],
                    sbo.correct = sbo_results$correct[[y]],
                    # Forest
                    rf.prediction = rf.prediction[[y]],
                    rf.correct = rf.correct[[y]]
                  )
                )
              }
              if (y.minus > 0) {
                return(
                  list(
                    # N Gram
                    mean.res.single = mean(outcome.simple[[y]] %>% unlist(), na.rm = T),
                    mean.res.three = mean(outcome.triple[[y]] %>% unlist(), na.rm = T),
                    sample.size = unlist(dataset.test$consequent) %>% length(),
                    expected = unlist(dataset.test$consequent),
                    observed = sapply(purrr::transpose(pred)$simple.sample, function(k) {
                      k[[y]]
                    }),
                    probs.expected = probs.expected[[y]],
                    # Naive Bayes
                    nb.prediction = nb.prediction[[y]],
                    nb.expected = as.character(train_sets[[y.minus]]$test.set[, 1]),
                    nb.correct = nb.correct[[y]],
                    # SBO
                    sbo.prediction = sbo_results$prediction[[y]],
                    sbo.correct = sbo_results$correct[[y]],
                    # Forest
                    rf.prediction = rf.prediction[[y]],
                    rf.correct = rf.correct[[y]]
                  )
                )
              }
            })
          names(all_out) <- 0:lvl
          
          return(all_out)
        })
        
        
        ## if nothing happened, return NAs
        if (mean(is.na(unlist(ran.matrixes))) == 1) {
          return(lapply(1:lvl, function(y) {
            list(
              mean.res.single = NA,
              mean.res.three = NA,
              sample.size = NA,
              expected = NA,
              observed = NA,
              probs.expected = NA,
              nb.prediction = NA,
              nb.expected = NA,
              nb.correct = NA,
              sbo.prediction = NA,
              sbo.correct = NA,
              rf.prediction = NA,
              rf.correct = NA
            )
          }))
        }
        
        # transpose all trials
        ran.matrixes <- transpose(ran.matrixes)
        
        
        # mean correct predictions
        mean.res.single <-
          lapply(seq_along(ran.matrixes), function(y) {
            mean(unlist(purrr::transpose(ran.matrixes[[y]])$mean.res.single), na.rm = T)
          })
        # mean correct predictions in top three
        mean.res.three <-
          lapply(seq_along(ran.matrixes), function(y) {
            mean(unlist(purrr::transpose(ran.matrixes[[y]])$mean.res.three), na.rm = T)
          })
        # mean sample size
        sample.size <-
          lapply(seq_along(ran.matrixes), function(y) {
            mean(unlist(purrr::transpose(ran.matrixes[[y]])$sample.size), na.rm = T)
          })
        # preserve observed and expected values
        observed <-
          lapply(seq_along(ran.matrixes), function(y) {
            unlist(purrr::transpose(ran.matrixes[[y]])$observed)
          })
        expected <-
          lapply(seq_along(ran.matrixes), function(y) {
            unlist(purrr::transpose(ran.matrixes[[y]])$expected)
          })
        probs.expected <-
          lapply(seq_along(ran.matrixes), function(y) {
            unlist(purrr::transpose(ran.matrixes[[y]])$probs.expected)
          })
        nb.observed <-
          lapply(seq_along(ran.matrixes), function(y) {
            unlist(purrr::transpose(ran.matrixes[[y]])$nb.prediction)
          })
        nb.expected <-
          lapply(seq_along(ran.matrixes), function(y) {
            unlist(purrr::transpose(ran.matrixes[[y]])$nb.expected)
          })
        nb.correct <-
          lapply(seq_along(ran.matrixes), function(y) {
            mean(unlist(purrr::transpose(ran.matrixes[[y]])$nb.correct), na.rm = T)
          })
        sbo.observed <-
          lapply(seq_along(ran.matrixes), function(y) {
            unlist(purrr::transpose(ran.matrixes[[y]])$sbo.prediction)
          })
        sbo.correct <-
          lapply(seq_along(ran.matrixes), function(y) {
            mean(unlist(purrr::transpose(ran.matrixes[[y]])$sbo.correct), na.rm = T)
          })
        rf.observed <-
          lapply(seq_along(ran.matrixes), function(y) {
            unlist(purrr::transpose(ran.matrixes[[y]])$rf.prediction)
          })
        rf.correct <-
          lapply(seq_along(ran.matrixes), function(y) {
            mean(unlist(purrr::transpose(ran.matrixes[[y]])$rf.correct), na.rm = T)
          })
        
        all_res <-
          lapply(seq_along(ran.matrixes), function(y) {
            list(
              accuracy = mean.res.single[[y]],
              accuracy.top.three = mean.res.three[[y]],
              sample.size = sample.size[[y]],
              observed = observed[[y]],
              expected = expected[[y]],
              probs.expected = probs.expected[[y]],
              naivebayes.observed = nb.observed[[y]],
              naivebayes.expected = nb.expected[[y]],
              naivebayes.accuracy = nb.correct[[y]],
              sbo.accuracy = sbo.correct[[y]],
              sbo.observed = sbo.observed[[y]],
              forest.observed = rf.observed[[y]],
              forest.accuracy = rf.correct[[y]]
            )
          })
        names(all_res) <- 0:lvl
        
        return(all_res)
      }
    )
  stopCluster(mycluster)
  
  # remove empty runs
  
  loo.pred <- loo.pred[sapply(seq_along(loo.pred), function(x){mean(!is.na(unlist(loo.pred[[x]])))})]
  
  # transpose lists
  loo_nn <- purrr::transpose(loo.pred)
  
  
  # Evaluation Model Choice -------------------------------------------------
  all_evaluation <- lapply(seq_along(loo_nn), function(y) {
    loo_nn_x <- purrr::transpose(loo_nn[[y]])
    # select all elements as data frame and how often they were expected
    results <-
      data.frame(t(table(unlist(
        loo_nn_x$expected
      ))))[, 2:3]
    colnames(results) <- c("element", "expected")
    # account for number of trials
    results$expected <- round(results$expected / trials, 3)
    # define observed
    obs <- t(table(unlist(loo_nn_x$observed)))[1, ] / trials
    results$observed <-
      round(obs[match(results$element, names(obs))], 3)
    # define correct
    correct <-
      t(table(unlist(loo_nn_x$observed)[unlist(loo_nn_x$observed) == unlist(loo_nn_x$expected)]))[1, ] / trials
    results$correct <-
      correct[match(results$element, names(correct))]
    
    # set NA as 0s
    results$observed <-
      ifelse(is.na(results$observed), 0, results$observed)
    results$correct <-
      ifelse(is.na(results$correct), 0, results$correct)
    results$true.positive <-
      round(results$correct / results$expected, 3)
    
    # determine number of false positions
    false.pos <-
      t(table(unlist(loo_nn_x$observed)[unlist(loo_nn_x$observed) != unlist(loo_nn_x$expected)]))[1, ] / trials
    false.positive <-
      false.pos[match(results$element, names(false.pos))]
    false.positive <-
      ifelse(is.na(false.positive), 0, false.positive)
    results$false.positive <-
      round(false.positive / results$observed, 3)
    
    # define number of false negatives
    false.neg <-
      t(table(unlist(loo_nn_x$expected)[unlist(loo_nn_x$observed) != unlist(loo_nn_x$expected)]))[1, ] / trials
    false.negative <-
      false.neg[match(results$element, names(false.neg))]
    false.negative <-
      ifelse(is.na(false.negative), 0, false.negative)
    results$false.negative <-
      round(false.negative / results$expected, 3)
    
    
    # Misclassification Matrix ------------------------------------------------
    
    # make misclassification plot
    misclass <- data.frame(t(table(
      unlist(loo_nn_x$expected),
      unlist(loo_nn_x$observed)
    )))
    colnames(misclass) <- c("expected", "observed", "count")
    tot.exp <- misclass %>%
      group_by(.data$expected) %>%
      summarise(tot.exp = sum(.data$count))
    
    misclass <- misclass %>%
      left_join(tot.exp) %>%
      mutate(misclassification.probability = round(.data$count / tot.exp, 3)) %>%
      select(-.data$tot.exp) %>%
      suppressMessages()
    
    mis.matr <-
      graph_from_edgelist(as.matrix(misclass[, c("expected", "observed")]),
                          directed = T
      )
    edge.attributes(mis.matr)$probability <-
      misclass$misclassification.probability
    mis.matr <-
      mis.matr %>%
      get.adjacency(attr = "probability") %>%
      as.matrix()
    
    mis.plot <- ggplot(
      data = misclass,
      aes(
        x = .data$expected,
        y = .data$observed,
        fill = .data$misclassification.probability
      )
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
        "Misclassification Heat Map, Level = ", y
      ), collapse = " "))
    
    # Misclassification for Naive Bayes
    if (y > 1) {
      misclass.nb <- data.frame(t(table(
        unlist(loo_nn_x$naivebayes.expected),
        unlist(loo_nn_x$naivebayes.observed)
      )))
      colnames(misclass.nb) <- c("observed", "expected", "count")
      tot.exp <- misclass.nb %>%
        group_by(.data$expected) %>%
        summarise(tot.exp = sum(.data$count))
      
      misclass.nb <- misclass.nb %>%
        left_join(tot.exp) %>%
        mutate(misclassification.probability = round(.data$count / tot.exp, 3)) %>%
        select(-.data$tot.exp) %>%
        suppressMessages()
      
      mis.matr.nb <-
        graph_from_edgelist(as.matrix(misclass.nb[, c("expected", "observed")]),
                            directed = T
        )
      edge.attributes(mis.matr.nb)$probability <-
        misclass.nb$misclassification.probability
      mis.matr.nb <-
        mis.matr.nb %>%
        get.adjacency(attr = "probability") %>%
        as.matrix()
      
      mis.plot.nb <- ggplot(
        data = misclass.nb,
        aes(
          x = .data$expected,
          y = .data$observed,
          fill = .data$misclassification.probability
        )
      ) +
        geom_tile() +
        scale_fill_gradient2(
          low = "white",
          high = "red",
          limit = c(0, max(
            misclass.nb$misclassification.probability
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
          "Misclassification Heat Map, Level = ", y
        ), collapse = " "))
    }
    if (y == 1) {
      mis.matr.nb <- NA
      mis.plot.nb <- NA
    }
    
    return(
      list(
        accuracy = mean(unlist(loo_nn_x$accuracy), na.rm = T),
        naivebayes.accuracy = mean(unlist(loo_nn_x$naivebayes.accuracy), na.rm = T),
        sbo.accuracy = mean(unlist(loo_nn_x$sbo.accuracy), na.rm = T),
        forest.accuracy = mean(unlist(loo_nn_x$forest.accuracy), na.rm = T),
        accuracy.top.three = mean(unlist(loo_nn_x$accuracy.top.three), na.rm = T),
        loglik = sum(log(unlist(
          loo_nn_x$probs.expected
        ))),
        sample.size = mean(unlist(loo_nn_x$sample.size), na.rm = T),
        element.results = results,
        confusion.matrix = mis.matr,
        heatmap = mis.plot,
        naivebayes.confusion.matrix = mis.matr.nb,
        naivebayes.heatmap = mis.plot.nb
      )
    )
  })
  
  names(all_evaluation) <- 0:lvl
  
  return(all_evaluation)
}
