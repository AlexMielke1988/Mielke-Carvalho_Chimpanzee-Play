#' Test robustness and distribution of element transitions. Randomly select subsets of the bouts and create bootstrapped information on Antecedent -> consequent transitions. Extracts all different ways (sum of transitions, conditional probability, joint probability, point-wise mutual information), and returns either raw bootstraps or standard deviation, credible range, and coefficient of variance
#'
#' @param elem.bout list of vectors of elements. Co-occurring elements are marked by the percentage symbol
#' @param antecedent first element in transition
#' @param consequent second element in transition
#' @param measure which measure to quantify the antecedent - consequent transition probability: 'sum' gives the number of transitions, 'prob' gives conditional probability, 'joint.prob' gives joint probability, 'mi' gives point-wise mutual information
#' @param it number of iterations of assignments
#' @param cores number of cores for parallel randomization (default is 2)
#' @param trials number of random trials to calculate expected values and z, p etc
#' @param ci.range range of credible interval extracted from the bootstraps; default is ci.range = c(0.025, 0.975), which means the bootstrapped range excluding the most extreme 2.5 percentage at each tail
#' @param output default is 'expected' (give only summary values across all randomizations); can be set to 'raw' (give the value for all randomizations)
#' @param ran.method either 'sample' (just sample one of the co-occurring) or 'random' (randomizes their order)
#'
#'
#' @return Returns either raw values from bootstraps or list with credible range, sd, and coefficient of variance for 'sum' (sum of transitions), 'prob' (probability of transition), 'joint.prob' (joint probability), or 'mi' (point-wise mutual information)
#'
#' @importFrom dplyr mutate arrange desc as_tibble select all_of na_if inner_join left_join bind_rows bind_cols rename mutate_all distinct row_number summarize tibble filter count distinct
#' @importFrom stringr str_detect str_split str_replace_all str_to_title str_to_lower
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na gather separate unite
#' @importFrom parallel parLapply stopCluster mclapply makeCluster detectCores clusterExport
#' @importFrom doParallel registerDoParallel
#'
#'
#' @author Alex Mielke
#' @export
#'
boot_elements <- function(elem.bout,
                          antecedent,
                          consequent,
                          measure = c("sum", "prob", "mi", "joint.prob"),
                          it = 20,
                          cores = 2,
                          trials = 1000,
                          ci.range = c(0.025, 0.975),
                          output = "summary",
                          ran.method = 'random') {

  # remove unnecessary strings
  elem.bout <- elem.bout %>%
    lapply(str_replace_all, pattern = '-|/|_', replacement = '')


  # prepare parallel
  mycluster <- makeCluster(cores, type = "PSOCK")
  # export the relevant information to each core
  clusterExport(
    cl = mycluster,
    c(
      "elem.bout",
      "antecedent",
      "consequent",
      "unlist_list",
      "unlist_vector",
      "elem_info",
      "elem_bout_matrix",
      'ran.method',
      "it",
      "output",
      "ci.range",
      "measure",
      "%>%",
      "str_replace_all",
      "tibble",
      "unnest_tokens",
      "dt_separate",
      "bind_rows",
      "cur_data_all"
    ),
    envir = environment()
  )
  registerDoParallel(mycluster)

  # run parallel loop

  # one loop for each 'trial'
  boot.probs <- parLapply(mycluster,
                       X = 1:trials, function(x) {
    # select subset of bouts using replacement
    boot_sample <- unique(sample(seq_along(elem.bout), replace = T))
    ran.bouts <- elem.bout[boot_sample]

    # calculate measure of interest and save
    ran.measure <- elem_info(
      antecedent = antecedent,
      consequent = consequent,
      elem.bout = ran.bouts,
      it = it,
      measure = measure,
      ran.method = ran.method
    ) %>% unlist(FALSE, FALSE)
    return(ran.measure)
  })
  stopCluster(mycluster)

  # bind bootstraps
  boot.probs <- do.call(cbind, boot.probs)

  if (output == "raw") {
    return(boot.probs)
  }

  # calculate measures
  lower.ci <- apply(boot.probs, 1, quantile, probs = ci.range[1], na.rm = T)
  upper.ci <- apply(boot.probs, 1, quantile, probs = ci.range[2], na.rm = T)
  range.ci <- upper.ci - lower.ci
  sd.ci <- apply(boot.probs, 1, sd, na.rm = T)
  cv.ci <- sd.ci / rowMeans(boot.probs, na.rm = T)

  return(list(
    lower.ci = lower.ci,
    upper.ci = upper.ci,
    range.ci = range.ci,
    sd.ci = sd.ci,
    cv.ci = cv.ci
  ))
}
