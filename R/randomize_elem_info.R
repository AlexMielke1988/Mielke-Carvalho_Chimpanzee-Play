#' Randomize bouts and extract information on expected Antecedent -> consequent transitions. Extracts all different ways (sum of transitions, conditional probability, joint probability, point-wise mutual information)
#'
#' @param elem.bout list of vectors of elements. Co-occurring elements are marked by the percentage symbol.
#' @param antecedent first element in transition
#' @param consequent second element in transition
#' @param observed vector of observed probabilities to calculate z and p-values
#' @param it number of iterations of assignments
#' @param cores number of cores for parallel randomization (default is 2)
#' @param trials number of random trials to calculate expected values and z, p etc
#' @param type how to determine probability of elements for randomization. If 'across', frequencies of elements are the same across all bouts, while 'within' shuffles elements within a bout
#' @param output default is 'expected' (give only summary values across all randomizations); can be set to 'raw' (give the value for all randomizations)
#' @param ran.method for co-occurring elements, how should they be split? Can be 'random' (order is randomised), or 'sample' (randomly choose any)
#'
#'
#' @return Returns a list with 'sum' (expected sum of transitions), 'prob' (expected probability of transition), 'joint' (expected joint probability), 'p-value' (comparison of observed with all randomizations), 'z' (z value of observed against all randomizations)
#'
#' @importFrom dplyr mutate arrange desc as_tibble select all_of na_if inner_join left_join bind_rows bind_cols rename mutate_all distinct row_number summarize tibble filter count distinct
#' @importFrom stringr str_detect str_split str_replace_all str_to_title str_to_lower
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na gather separate unite
#' @importFrom reshape2 acast
#' @importFrom parallel parLapply stopCluster mclapply makeCluster detectCores clusterExport
#' @importFrom doParallel registerDoParallel
#'
#'
#' @author Alex Mielke
#' @export
#'
#' @examples
#' ###
randomized_elem_info <- function(elem.bout,
                                 antecedent,
                                 consequent,
                                 observed = NULL,
                                 it = 20,
                                 cores = 2,
                                 trials = 1000,
                                 type = "across",
                                 output = "expected",
                                 ran.method = 'random') {

  # remove unnecessary strings
  elem.bout <- elem.bout %>%
    lapply(str_replace_all, pattern = '-|/|_', replacement = '')


  # prepare summary of element-level info for all bouts
  all.elements <- as.vector(unlist(unlist_list(elem.bout, method = 'random')))
  all.elements <- data.frame(t(table(all.elements)))

  # match counts and probabilities to order of info
  count.antecedent <- all.elements$Freq[match(antecedent, all.elements$all.elements)]
  count.consequent <- all.elements$Freq[match(consequent, all.elements$all.elements)]

  prob.antecedent <- count.antecedent / (length(all.elements) - length(elem.bout))
  prob.consequent <- count.consequent / (length(all.elements) - length(elem.bout))

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
      "elem_bout_matrix",
      "randomize_bouts",
      "elem_info",
      "observed",
      "type",
      "it",
      "output",
      'ran.method',
      "prob.consequent",
      "prob.antecedent"
    ),
    envir = environment()
  )
  registerDoParallel(mycluster)

  # run parallel loop
  randomized.probs <- lapply(1:trials, function(x) {
    # randomise bout orders across or within bouts
    ran.bouts <- randomize_bouts(elem.bout = elem.bout, type = type)

    # calculate transitions
    element.bout.sum <- data.frame(
      elem_bout_matrix(elem.bout = ran.bouts,
                            it = it,
                            ran.method = ran.method), check.names = F)

    # calculate element-wise counts
    Aall <- rowSums(element.bout.sum)[match(antecedent, as.character(rownames(element.bout.sum)))]
    Ball <- colSums(element.bout.sum)[match(consequent, as.character(colnames(element.bout.sum)))]

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

    AnotB <- Aall - AandB
    BnotA <- Ball - AandB
    notAnotB <- sum(element.bout.sum) - Ball - Aall


    # create variables
    ran.sum <- AandB
    ran.joint <- AandB / sum(element.bout.sum)
    ran.prob <- AandB / Aall
    ran.mi <-
      (log(
        (AandB / sum(element.bout.sum)) /
          (Aall / sum(element.bout.sum) *
            Ball / sum(element.bout.sum))
      )
      ) /
        (-1 * log(AandB / sum(element.bout.sum))
        )


    return(list(
      sum = ran.sum,
      prob = ran.prob,
      mi = ran.mi,
      joint.prob = ran.joint
    ))
  })
  stopCluster(mycluster)

  randomized.probs <- purrr::transpose(randomized.probs)

  ran.prob <- do.call(cbind, randomized.probs$prob)
  ran.sum <- do.call(cbind, randomized.probs$sum)
  ran.mi <- do.call(cbind, randomized.probs$mi)
  ran.joint <- do.call(cbind, randomized.probs$joint.prob)


  if (output == "raw") {
    return(list(
      prob = ran.prob,
      sum = ran.sum,
      mi = ran.mi,
      joint.prob = ran.joint
    ))
  }

  ######### compare randomized and observed
  probs <- rowMeans(ran.prob, na.rm = T)
  probs[is.na(probs)] <- 0

  sums <- rowMeans(ran.sum, na.rm = T)
  sums[is.na(sums)] <- 0

  mi <- rowMeans(ran.mi, na.rm = T)
  mi[is.na(mi)] <- 0

  joint <- rowMeans(ran.joint, na.rm = T)
  joint[is.na(joint)] <- 0

  pvalue <- sapply(1:length(antecedent), function(x) {
    mean(observed[x] <= ran.prob[x, ])
  })
  pvalue[is.na(pvalue)] <- 0.5

  z <- sapply(1:length(antecedent), function(x) {
    (observed[x] - mean(ran.prob[x, ])) / sd(ran.prob[x, ])
  })
  z[is.na(z)] <- 0.5

  return(list(
    sum = sums,
    prob = probs,
    mi = mi,
    joint = joint,
    pvalue = pvalue,
    z = z
  ))
}
