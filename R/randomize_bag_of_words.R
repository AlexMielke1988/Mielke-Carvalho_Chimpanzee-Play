#' Randomize bouts and extract information on expected bag-of-words
#'
#' @param elem.bout list of vectors of elements. Co-occurring elements are marked by the percentage symbol.
#' @param elem.time list of vectors of timings of elements. If set to NULL, the order of elements is used with distance of 1
#' @param observed vector of observed probabilities to calculate z and p-values
#' @param gap number of elements to each side that should be counted in the same 'bag' as the element; if 'all', use whole bout
#' @param cores number of cores for parallel randomization (default is 2)
#' @param trials number of random trials to calculate expected values and z, p etc
#' @param output default is 'expected' (give only summary values across all randomizations); can be set to 'raw' (give the value for all randomizations)
#'
#'
#' @return Returns a list with 'sum' (expected sum of transitions), 'prob' (expected probability of transition), 'joint' (expected joint probability), 'p-value' (comparison of observed with all randomizations), 'z' (z value of observed against all randomizations)
#'
#' @importFrom parallel parLapply
#' @importFrom parallel mclapply
#' @importFrom parallel makeCluster
#' @importFrom parallel detectCores
#' @importFrom parallel clusterExport
#' @importFrom parallel stopCluster
#'
#' @author Alex Mielke
#' @export
#'
#' @examples
#' ###
randomized_bag_of_words <- function(elem.bout,
                                    elem.time,
                                    observed = NULL,
                                    gap = c(-1, 1),
                                    cores = 2,
                                    trials = 1000,
                                    output = "expected") {
  #go parallel
  mycluster <- makeCluster(cores, type = "PSOCK")
  # export the relevant information to each core
  clusterExport(
    cl = mycluster,
    c(
      "elem.bout",
      "bag_of_words",
      "unlist_vector",
      "unlist_list",
      "Reduce",
      "observed",
      "gap",
      "output",
      "randomize_bouts",
      "elem.time"
    ),
    envir = environment()
  )
  registerDoParallel(mycluster)

  # run parallel loop
  randomized.probs <-
    parLapply(cl = mycluster, X = 1:trials, function(x) {
      # use randomize_bouts function to do just that, across bouts
      ran.bouts <- randomize_bouts(elem.bout = elem.bout, type = "across")
      #use bag of words functions
      ran.bag <-
        bag_of_words(elem.bout = ran.bouts,
                     elem.time = elem.time,
                     gap = gap)

      # save outcome
      return(list(
        sum = ran.bag$observed.sum,
        prob = ran.bag$observed.probs,
        mi = ran.bag$mi
      ))
    })
  stopCluster(mycluster)

  #transpose list
  randomized.probs <- purrr::transpose(randomized.probs)

  # combine permutations
  ran.prob <- do.call(cbind, randomized.probs$prob)
  ran.sum <- do.call(cbind, randomized.probs$sum)
  ran.mi <- do.call(cbind, randomized.probs$mi)

  if (output == "raw") {
    return(list(prob = ran.prob,
                sum = ran.sum,
                mi = ran.mi))
  }

  ######### compare randomized and observed
  probs <- rowMeans(ran.prob, na.rm = T)
  probs[is.na(probs)] <- 0

  sums <- rowMeans(ran.sum, na.rm = T)
  sums[is.na(sums)] <- 0

  mi <- rowMeans(ran.mi, na.rm = T)
  mi[is.na(mi)] <- 0

  pvalue <- sapply(1:nrow(ran.prob), function(x) {
    mean(observed[x] <= ran.prob[x, ])
  })
  pvalue[is.na(pvalue)] <- 0.5

  z <- sapply(1:nrow(ran.prob), function(x) {
    (observed[x] - mean(ran.prob[x, ])) / sd(ran.prob[x, ])
  })
  z[is.na(z)] <- 0.5

  return(list(
    sum = sums,
    prob = probs,
    mi = mi,
    pvalue = pvalue,
    z = z
  ))
}
