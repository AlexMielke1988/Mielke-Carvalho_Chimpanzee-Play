#' Finds elements that show similar usage patterns, based on euclidean distance between their probabilities or mutual information with all other elements. Uses umap to reduce number of dimensions (bringing it down to 2) and k-means clustering to identify best number of clusters based on AIC values
#'
#' @param elem.bout list of vectors of elements. Co-occurring elements are marked by the percentage symbol.
#' @param it how many iterations of the random splitting of co-occurring elements should be used? If elements never co-occur, this can be set to 1. If there are many co-occurrences, this number should be high, but this will make things slower
#' @param k pre-defined cluster solution to be used; NULL if the best cluster solution (based on AIC) should be used
#' @param facet should plots be split by cluster? default is FALSE
#' @param measure should similarity be calculated based on co-occurrence in same bout ('prob') or point-wise mutual information ('mi')?
#' @param level Should the correlation/pmi be done on the level of the 'bout' (do two elements co-occur within a bout) or 'bigram' (they follow each other). Default is 'bigram'
#' @param ran.method for co-occurring elements, how should they be split? Can be 'random' (order is randomised), or 'sample' (randomly choose any)
#' @param n_epochs how many iterations for the UMAP dimension reduction?
#' @param trials number of times the cluster detection algorithm should be performed a robust solution
#'
#' @return Returns a list with three plots:
#' plot.antecedent: plot that shows similarity between elements in the conditional probabilities of which elements follow them in transitions
#' plot.descendant: plot that shows similarity between elements in the conditional probabilities of which elements precede them in transitions
#' plot.combined: plot that shows similarity between elements in the conditional probabilities of which elements precede and follow them in transitions
#'
#' @importFrom dplyr mutate arrange desc as_tibble select all_of na_if left_join bind_rows bind_cols rename mutate_all full_join
#' @importFrom stringr str_detect str_split str_replace_all str_to_title str_to_lower
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na gather separate unite
#' @importFrom ggplot2 ggplot geom_label aes ggtitle xlab ylab theme_classic annotate geom_hline position_jitter ylim qplot geom_text facet_wrap
#' @importFrom reshape2 acast
#' @importFrom dendextend get_subdendrograms hang.dendrogram set ladderize
#' @importFrom umap umap umap.defaults
#' @importFrom mclust Mclust mclustBIC
#' @importFrom ClusterR Optimal_Clusters_KMeans KMeans_rcpp
#' @importFrom stats as.dendrogram order.dendrogram dist hclust
#' @importFrom widyr pairwise_cor pairwise_pmi pairwise_count
#' @importFrom cluster silhouette pam
#'
#' @author Alex Mielke
#' @export
#'
#' @examples
#' ###
similiarity_clusters <- function(elem.bout,
                                 measure = c("mi", "prob", 'corr'),
                                 k = NULL,
                                 facet = FALSE,
                                 it = 10,
                                 level = 'bigram',
                                 ran.method = 'random',
                                 n_epochs = 7000,
                                 trials = 10) {
  # clean element names
  elem.bout <- elem.bout %>%
    lapply(str_replace_all,
           pattern = '-|/|_',
           replacement = '')
  
  # turn each bout into one string where elements are separated by spaces (needed for tidytext package). Repeat based on number of it that was specified
  elem.df <- lapply(1:it, function(x) {
    elem.df <-
      tibble(elem = as.vector(
        unlist(
          sapply(
            unlist_list(elem.bout,
                        method = ran.method),
            function(x) {
              paste(x, collapse = ' ')
            }))))
  })
  # bind list
  elem.df <- elem.df %>% bind_rows() %>%
    mutate(bout = seq_along(elem))
  
  # if the level of similarity is set to 'bout', similarity is based on co-occurrence of elements in bouts
  if (level == 'bout') {
    elem.corr = elem.df %>%
      # group by bout
      group_by(bout) %>%
      # use unnest_tokens to count elements
      unnest_tokens(word,
                    elem,
                    token = 'words', to_lower = FALSE) %>%
      ungroup() %>%
      # exclude NAs
      filter(.data$word != 'NA') %>%
      # use widyr to calculate pairwise correlations of words across bouts
      pairwise_cor(word,
                   bout) %>%
      # add same info for pairwise_pmi (pointwise mutual information)
      full_join(
        elem.df %>%
          group_by(.data$bout) %>%
          unnest_tokens(word,
                        elem,
                        token = 'words',
                        to_lower = FALSE) %>%
          ungroup() %>%
          filter(.data$word != 'NA') %>%
          pairwise_pmi(word,
                       bout)
      ) %>%
      # add same information for pairwise count
      full_join(
        elem.df %>%
          group_by(.data$bout) %>%
          unnest_tokens(word,
                        elem,
                        token = 'words',
                        to_lower = FALSE) %>%
          ungroup() %>%
          filter(.data$word != 'NA') %>%
          pairwise_count(word,
                         bout)
      ) %>%
      suppressMessages %>%
      mutate(n = .data$n / it)
    
    # count how often elements occur
    elem.sum <- elem.corr %>%
      select(.data$item1,
             .data$n) %>%
      group_by(.data$item1) %>%
      summarise(total.n = sum(.data$n, na.rm = T))
    
    # calculate probabilities
    elem.corr <- elem.corr %>%
      left_join(elem.sum) %>%
      mutate(probs = .data$n / .data$total.n) %>%
      arrange(.data$item1, .data$item2) %>%
      suppressMessages()
  }
  
  # for bigram level, do across all bouts and check how often A follows B
  if (level == 'bigram') {
    elem.corr = elem.df %>%
      # use unnest_tokens to calculate co-occurrences
      unnest_tokens(
        bigram,
        .data$elem,
        n = 2,
        token = 'ngrams',
        to_lower = FALSE
      ) %>%
      # gram number
      mutate(gram = seq_along(.data$bigram)) %>%
      group_by(.data$gram) %>%
      # how often do words occur in bigrams
      unnest_tokens(word,
                    bigram,
                    token = 'words', to_lower = FALSE) %>%
      ungroup() %>%
      #remove NA
      filter(.data$word != 'NA') %>%
      # calculate pairwise correlation of words within grams
      pairwise_cor(word,
                   gram) %>%
      full_join(
        # do same for pairwise_pmi
        elem.df %>%
          unnest_tokens(
            bigram,
            elem,
            n = 2,
            token = 'ngrams',
            to_lower = FALSE
          ) %>%
          mutate(gram = seq_along(.data$bigram)) %>%
          group_by(.data$gram) %>%
          unnest_tokens(word,
                        bigram,
                        token = 'words',
                        to_lower = FALSE) %>%
          ungroup() %>%
          filter(.data$word != 'NA') %>%
          pairwise_pmi(word,
                       gram)
      ) %>%
      full_join(
        # also add pairwise_count
        elem.df %>%
          unnest_tokens(
            bigram,
            elem,
            n = 2,
            token = 'ngrams',
            to_lower = FALSE
          ) %>%
          mutate(gram = seq_along(.data$bigram)) %>%
          group_by(.data$gram) %>%
          unnest_tokens(word,
                        bigram,
                        token = 'words',
                        to_lower = FALSE) %>%
          ungroup() %>%
          filter(.data$word != 'NA') %>%
          pairwise_count(word,
                         gram)
      ) %>%
      suppressMessages %>%
      mutate(n = .data$n / it)
    
    
    elem.sum <- elem.corr %>%
      select(.data$item1, .data$n) %>%
      group_by(.data$item1) %>%
      summarise(total.n = sum(.data$n, na.rm = T))
    
    elem.corr <- elem.corr %>%
      left_join(elem.sum) %>%
      mutate(probs = .data$n / .data$total.n) %>%
      arrange(.data$item1,
              .data$item2) %>%
      suppressMessages()
  }
  
  if (measure == "mi") {
    matrix.first <- elem.corr %>%
      acast(item1 ~ item2,
            value.var = 'pmi',
            drop = FALSE,
            fill = 0) %>% replace_na(0)
  }
  if (measure == "corr") {
    matrix.first <- elem.corr %>%
      acast(item1 ~ item2,
            value.var = 'correlation',
            drop = TRUE,
            fill = 0) %>% replace_na(0)
  }
  if (measure == "prob") {
    matrix.first <- elem.corr %>%
      acast(item1 ~ item2,
            value.var = 'probs',
            drop = TRUE,
            fill = 0) %>% replace_na(0)
  }
  
  if (measure == "count") {
    matrix.first <- elem.corr %>%
      acast(item1 ~ item2,
            value.var = 'n',
            drop = TRUE,
            fill = 0) %>%
      replace_na(0)
  }
  
  matrix.first <- matrix.first %>%
    dist(method = "manhattan") %>%
    as.matrix()
  
  # prepare UMAP algorith
  custom.config <- umap.defaults # set configuration to default
  custom.config$n_neighbors <-
    4 # set the number of nearest neighbours, based on the number of cases - with a maximum value of 15
  custom.config$n_epochs <-
    n_epochs # number of epochs for the UMAP algorithm
  custom.config$init <- 'random' # starting values set to random
  
  boot_clus <- lapply(1:trials, function(k) {
    custom.config$n_epochs <-
      sample(size = 1, rnorm(10, mean = n_epochs, sd = n_epochs / 10)) # number of epochs for the UMAP algorithm
    # if there are fewer than 3 modifier levels, set number of components to 2
    custom.config$n_components <- 2
    
    matrix.umap <- umap(
      matrix.first,
      random.state = 123,
      method = 'naive',
      config = custom.config
    )
    
    
    max.clus <- min(nrow(matrix.umap$layout) - 1, 20)
    opt.1 <- lapply(3:max.clus, function(x) {
      xx <- data.frame(
        clusters = x,
        silhouette = cluster::pam(matrix.umap$layout %>%
                                    as.matrix(), x) %>%
          cluster::silhouette(full = TRUE) %>%
          data.frame() %>%
          select(sil_width) %>%
          unlist() %>%
          mean(na.rm = T)
      )
      return(xx)
    }) %>%
      bind_rows() %>%
      select(silhouette) %>%
      unlist(F, F)
    opt.1 <- c(NA, NA, opt.1)
    
    return(which.max(opt.1))
  })
  
  
  k.1 <- names(boot_clus %>%
                 unlist() %>%
                 table)[boot_clus %>%
                          unlist() %>%
                          table %>%
                          which.max] %>%
    as.numeric()
  
  custom.config$n_components <- 2
  
  
  matrix.umap <- umap(
    matrix.first,
    random.state = 123,
    method = 'naive',
    config = custom.config
  )
  
  max.clus <- min(nrow(matrix.umap$layout) - 1, 20)
  opt.1 <- lapply(2:max.clus, function(x) {
    xx <- data.frame(
      clusters = x,
      silhouette = cluster::pam(matrix.umap$layout %>%
                                  as.matrix(), x) %>%
        cluster::silhouette(full = TRUE) %>%
        data.frame() %>%
        select(sil_width) %>%
        unlist() %>%
        mean(na.rm = T)
    )
    return(xx)
  }) %>%
    bind_rows() %>%
    select(silhouette) %>%
    unlist(F, F)
  opt.1 <- c(NA, NA, opt.1)
  # select best cluster solution and save it
  
  sil.1 <- cluster::pam(matrix.umap$layout %>%
                          as.matrix(), k.1) %>%
    cluster::silhouette(full = TRUE) %>%
    data.frame() %>%
    select(sil_width) %>%
    unlist() %>%
    mean(na.rm = T)
  # if k was determined in function call, use that
  if (!is.null(k)) {
    k.1 <- k
  }
  if (!is.null(k)) {
    sil.1 <- opt.1[k]
  }
  
  # if no silhouette is better than 0.3, select 1 as best cluster
  if (sil.1 < 0.3) {
    k.1 <- 1
  }
  
  if (k.1 == 0) {
    k.1 <- 1
  }
  # use K-Means clustering to assign clusters in the data
  umap.clust.1 <-
    cluster::pam(matrix.umap$layout %>%
                   as.matrix(), k.1)
  # add cluster membership to data
  matrix.1.umap.frame <- data.frame(matrix.umap$layout,
                                    cluster = as.factor(umap.clust.1$clustering))
  matrix.1.umap.frame$element <- rownames(matrix.1.umap.frame)
  
  
  p1 <-
    ggplot(
      matrix.1.umap.frame,
      aes(
        x = .data$X1,
        y = .data$X2,
        label = .data$element,
        color = .data$cluster
      )
    ) +
    geom_text(
      show.legend = F,
      position = position_jitter(width = 0.2, height = 0.2),
      fontface = 'bold',
      size = 3
    ) +
    ggtitle(paste(
      c(
        "Similarity; Best cluster solution: ",
        max(round(opt.1, 2), na.rm = T),
        "; number of clusters: ",
        k.1
      ),
      collapse = ""
    )) +
    theme_classic() +
    xlab("First Dimension") +
    ylab("Second Dimension")
  
  if (facet) {
    p1 <-
      ggplot(
        matrix.1.umap.frame,
        aes(
          x = .data$X1,
          y = .data$X2,
          label = .data$element,
          fill = .data$cluster
        )
      ) +
      geom_label(
        show.legend = F,
        position = position_jitter(width = 0.5, height = 0.5),
        size = 3
      ) +
      ggtitle(paste(
        c(
          "Similarity; Best cluster solution: ",
          max(round(opt.1, 2), na.rm = T),
          "; number of clusters: ",
          k.1
        ),
        collapse = ""
      )) +
      theme_classic() +
      xlab("First Dimension") +
      ylab("Second Dimension") +
      facet_wrap( ~ .data$cluster, scales = "free")
  }
  
  p2 <- qplot(
    x = 1:length(opt.1),
    y = opt.1,
    xlab = 'Clusters',
    ylab = 'AIC',
    main = 'Cluster Solutions - AIC'
  ) +
    geom_hline(mapping = aes(yintercept = min(opt.1)), linetype = 2) +
    geom_hline(mapping = aes(yintercept = min(opt.1) + 2),
               linetype = 3) +
    ylim(0, min(opt.1) + 50) +
    theme_classic()
  
  #### Dendogram approach
  
  # check if any silhouette is better than 0.3
  
  dend <- matrix.umap$layout %>%
    dist(method = 'manhattan') %>%
    hclust(method = 'average') %>%
    as.dendrogram() %>%
    set("branches_k_color", k = k.1) %>%
    hang.dendrogram(hang = 0.2) %>%
    ladderize() %>%
    set("labels_cex", c(.65))
  
  
  xx = get_subdendrograms(dend,
                          k = k.1,
                          order_clusters_as_data = TRUE)
  
  dend_cluster <-
    lapply(1:length(xx), function(x) {
      data.frame(element =
                   as.matrix(matrix.first[order.dendrogram(xx[[x]]),-5]) %>%
                   rownames() %>% unlist,
                 cluster = x)
    }) %>%
    bind_rows() %>%
    left_join(p1$data, by = c('element' = 'element')) %>%
    select(-.data$X1,-.data$X2) %>%
    arrange(.data$cluster.y)
  colnames(dend_cluster) = c('element', 'dendrogram.cluster', 'k.means.cluster')
  
  return(
    list(
      plot.similarity = p1,
      solutions = opt.1,
      plot.solutions = p2,
      custom.config = custom.config,
      dendrogram = dend,
      dendrogram.plot = ggplot(dend, horiz = TRUE),
      clusters = dend_cluster
    )
  )
}
