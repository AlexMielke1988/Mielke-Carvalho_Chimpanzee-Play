#' Create dataframe that summarises how often each individual element occurs in the dataset; basis for many of the other functions
#'
#' @param elem.bout list of vectors of elements. Co-occurring elements are marked by the percentage symbol.
#' @param elements vector of all elements that should be included in the analysis; if 'NULL', the function takes all elements that occur in the elem.bout list
#'
#' @return Function returns a data frame with each antecedent, consequent, and their individual occurrence stats
#'
#' @importFrom dplyr mutate arrange desc as_tibble select all_of na_if inner_join left_join bind_rows bind_cols rename mutate_all distinct row_number summarize tibble filter count distinct
#' @importFrom stringr str_detect str_split str_replace_all str_to_title str_to_lower
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na gather separate unite
#' @importFrom tidytext unnest_tokens
#' @importFrom tidyfast dt_separate
#' @importFrom purrr map
#'
#' @author Alex Mielke
#' @export
#'
#' @examples
#' ###
collocation_analysis <- function(elem.bout,
                                 lvl = 1,
                                 it = 1,
                                 ran.method = "random") {
  # define level
  lvl.used = lvl
  if(lvl == 0){lvl.used = 1}
  # create vector with all unique elements
  elements <-
    sort(unique(unlist(
      strsplit(as.character(unlist(elem.bout, FALSE, FALSE)), split = "%", fixed = T),
      FALSE, FALSE
    )))
  
  # multiplicate vector by number of it for randomisation
  elem.bout.combs <- rep(elem.bout, it)
  
  # add NAs at the end of every sequence
  elem.bout.combs <- lapply(elem.bout.combs, function(x) {
    c(x, rep(NA, lvl.used))
  })
  
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
    summarize(Freq = n()/it) %>%
    ungroup() %>%
    suppressMessages()
  
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
  
  # sort it all
  all.combs$combination <- as.character(all.combs$combination)
  all.combs <- all.combs %>%
    arrange(.data$antecedent, .data$consequent)
  
  antecedent_table = cbind(antecedent = elem.combs$antecedent,
                           consequent = elem.combs$consequent) %>% 
    data.frame()
  
  collexemes <- dist.collexemes(antecedent_table = antecedent_table,
                                it = it)
  
  all.combs <- all.combs %>% 
    left_join(collexemes) %>% 
    select(-observed) %>% 
    suppressMessages()
  
  return(all.combs)
}




# Coll.analysis V 3.2a
# Collostructional analysis: Computing the degree of association between words and words/constructions
# Coyright (C) 2007 Stefan Th. Gries (Latest changes in this version: 03/28/2010)

dist.collexemes <- function(antecedent_table, it = 1) { # FUNCTION FOR DISTINCTIVE COLLEXEME ANALYSIS

  mdca.data <- antecedent_table
    
  names(mdca.data)<-c("W_C", "Coll_Word")
  
  mdca.data <- mdca.data %>% 
    arrange(W_C, Coll_Word)
    
    # determine column frequencies
  tab.mca.data<-table(mdca.data$Coll_Word, mdca.data$W_C) # generate table for multiple dca
  colfreq<-table(mdca.data$W_C)
  verb<-rownames(tab.mca.data); constr<-colnames(tab.mca.data)
  n.verb<-length(verb); n.constr<-length(constr)
    
  result.table<-data.frame(matrix(nrow=n.verb, ncol=(n.constr)+1))
  colnames(result.table)<-c("Coll_Word", as.character(constr))
  result.table[,1]<-rownames(tab.mca.data)
  result.table[,2:(n.constr+1)]<-tab.mca.data[,1:n.constr]
  
  result.table <- lapply(1:n.verb, function(f){
    
    res.table <- data.frame(consequent = rownames(tab.mca.data)[f],
                            antecedent = constr)
    
    res.table$observed<-tab.mca.data[f,]
    res.table$expected<-as.vector(round(sum(res.table$observed)*(colfreq/sum(colfreq)), 3))
    
    counter<-0
    for (g in 1:nrow(res.table)) {
      counter<-counter+1
      if (res.table$observed[counter]>=res.table$expected[counter]) {
        res.table$pbin[g]<-round(-log(sum(dbinom(res.table$observed[counter]:sum(res.table$observed), sum(res.table$observed), (res.table$expected[counter]/sum(res.table$observed)))), 10), 3)
      } else {
        res.table$pbin[g]<-round(log(sum(dbinom(0:res.table$observed[counter], sum(res.table$observed), (res.table$expected[counter]/sum(res.table$observed)))), 10), 3)
      }
    }
    
    res.table$observed <- res.table$observed/it
    res.table$expected <- res.table$expected/it
    res.table$pbin <- res.table$pbin/it
    
    
    return(res.table)
  }) %>% 
    bind_rows()
  
  
  
  # output
  result.table<-as.data.frame(result.table %>% 
                                arrange(antecedent, consequent))
  
  return(result.table)
  
} # END OF FUNCTION FOR DISTINCTIVE COLLEXEME ANALYSIS
