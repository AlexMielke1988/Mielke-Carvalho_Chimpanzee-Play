#' Bootstrap bouts and extract information on expected bag-of-words
#'
#' @param elem.bout list of vectors of elements. Co-occurring elements are marked by the percentage symbol.
#' @param elem.time list of vectors of timings of elements. If set to NULL, the order of elements is used with distance of 1
#' @param measure which measure to quantify the antecedent - consequent transition probability: 'sum' gives the number of transitions, 'prob' gives conditional probability, 'joint.prob' gives joint probability, 'mi' gives point-wise mutual information
#' @param gap number of elements to each side that should be counted in the same 'bag' as the element; if 'all', use whole bout
#' @param cores number of cores for parallel randomization (default is 2)
#' @param trials number of random trials to calculate bootstrapped values
#' @param ci.range range of credible interval extracted from the bootstraps; default is ci.range = c(0.025, 0.975), which means the bootstrapped range excluding the most extreme 2.5 percentage at each tail
#' @param output default is 'expected' (give only summary values across all randomizations); can be set to 'raw' (give the value for all randomizations)
#'
#'
#' @return Returns a list with 'sum' (expected sum of transitions), 'prob' (expected probability of transition), 'joint' (expected joint probability), 'p-value' (comparison of observed with all randomizations), 'z' (z value of observed against all randomizations)
#'
#' @importFrom parallel parLapply stopCluster mclapply makeCluster detectCores clusterExport
#' @importFrom doParallel registerDoParallel
#'
#' @author Alex Mielke
#' @export
#'
#'
boot_bag_of_words <- function(elem.bout,
                              elem.time,
                              measure = c("sum", "prob", "mi"),
                              gap = c(-1, 1),
                              cores = 2,
                              trials = 1000,
                              ci.range = c(0.025, 0.975),
                              output = "summary") {

  # go parallel
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
      "gap",
      "output",
      "randomize_bouts",
      "elem.time",
      "measure",
      "ci.range"
    ),
    envir = environment()
  )
  registerDoParallel(mycluster)
  # run parallel loop, one for each set trial

  randomized.probs <- lapply(1:trials, function(x) {
    # randomly select some bouts with replacement
    boot_sample <- unique(sample(names(elem.bout), replace = T))
    # set those bouts as the ones in question
    ran.bouts <- elem.bout[boot_sample]
    ran.time <- elem.time[boot_sample]
    # calculate bag of words distribution within those selected bouts
    ran.bag <- bag_of_words(
      elem.bout = ran.bouts,
      elem.time = ran.time,
      gap = gap
    )

    if (measure == "sum") {
      prob <- ran.bag$observed.sum
    }
    if (measure == "prob") {
      prob <- ran.bag$observed.probs
    }
    if (measure == "mi") {
      prob <- ran.bag$mi
    }

    return(list(prob = prob))
  })
  stopCluster(mycluster)

  boot.probs <- lapply(randomized.probs, function(x) {
    x$prob
  })
  boot.probs <- do.call(cbind, boot.probs)

  if (output == "raw") {
    return(boot.probs)
  }

  lower.ci <-
    apply(boot.probs, 1, quantile, probs = ci.range[1], na.rm = T)
  upper.ci <-
    apply(boot.probs, 1, quantile, probs = ci.range[2], na.rm = T)
  range.ci <- upper.ci - lower.ci
  sd.ci <- apply(boot.probs, 1, sd, na.rm = T)
  cv.ci <- sd.ci / rowMeans(boot.probs, na.rm = T)

  return(
    list(
      lower.ci = lower.ci,
      upper.ci = upper.ci,
      range.ci = range.ci,
      sd.ci = sd.ci,
      cv.ci = cv.ci
    )
  )
}
