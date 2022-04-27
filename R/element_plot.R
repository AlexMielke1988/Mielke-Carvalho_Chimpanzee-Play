#' Create ego-network plot for an element - plots the significant antecedents and consequents for this element
#'
#' @param element element name
#' @param antecedent antecedent element in transition
#' @param consequent consequent element in transition
#' @param count.antecedent number of times the antecedent occurred (output of 'transition.frame')
#' @param count.consequent number of times the consequent occurred (output of 'transition.frame')
#' @param observed.probs observed conditional probability to go from antecedent to consequent (output of 'elem.info')
#' @param pvalue pvalue of observed against randomized expected probabilities (output of 'randomized.elem.info')
#' @param cutoff number of times transitions should have been observed to be included (default is cutoff = 5)
#' @param significance level of significance for transitions to be considered significantly more common than expected; default is significance = 0.01
#'
#' @return Returns a ggplot element indicating the antecedents, consequents, and probabilities in each direction
#'
#' @importFrom dplyr mutate arrange desc as_tibble select all_of na_if left_join bind_rows bind_cols rename mutate_all
#' @importFrom stringr str_detect str_split str_replace_all str_to_title str_to_lower
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na gather separate unite
#' @importFrom ggplot2 ggplot geom_label aes ggtitle xlab ylab theme_classic annotate geom_hline coord_fixed scale_size_continuous arrow unit
#' @importFrom ggraph ggraph create_layout geom_node_text scale_edge_alpha theme_graph geom_edge_fan geom_node_label geom_edge_link geom_edge_loop circle
#' @importFrom igraph vertex.attributes vertex.attributes<- add_vertices V edge.attributes edge.attributes<- graph_from_data_frame graph.adjacency delete_edges add_vertices get.data.frame bipartite_mapping cluster_fast_greedy modularity V<- graph_from_edgelist
#' @importFrom grDevices rainbow
#' @importFrom graphics layout
#' @importFrom rlang .data
#'
#' @author Alex Mielke
#' @export
#'
#' @examples
#' ###
element_plot <-
  function(element,
           antecedent,
           consequent,
           count.antecedent,
           count.consequent,
           observed.probs,
           pvalue,
           cutoff = 5,
           significance = 0.01) {
    # put together transition information
    transitions <- data.frame(
      antecedent = antecedent,
      consequent = consequent,
      count.antecedent = count.antecedent,
      count.consequent = count.consequent,
      observed.probs = observed.probs,
      pvalue = pvalue
    )

    # information about the element as antecedent
    element.antecedent <-
      transitions[transitions$antecedent == element &
        transitions$count.consequent > cutoff, ]
    # information about the element as consequent
    element.consequent <-
      transitions[transitions$consequent == element &
        transitions$count.antecedent > cutoff, ]

    # select bigrams that are more common than expected
    antecedents <- element.consequent$antecedent[element.consequent$pvalue <= significance]
    consequents <- element.antecedent$consequent[element.antecedent$pvalue <= significance]

    # prepare network
    ante <- c(antecedents, rep(element, length(consequents)))
    conse <- c(rep(element, length(antecedents)), consequents)
    probs <-
      c(
        element.consequent$observed.probs[element.consequent$pvalue <= significance],
        element.antecedent$observed.probs[element.antecedent$pvalue <= significance]
      )
    #create network with igraph
    egonet <- graph_from_edgelist(cbind(ante, conse), directed = T)
    edge.attributes(egonet)$observed.prob <- probs
    V(egonet)$size <- rep(1, length(vertex.attributes(egonet)$name))
    V(egonet)$type <-
      ifelse(vertex.attributes(egonet)$name %in% antecedents,
        "antecedent",
        "consequent"
      )

    # set network attributes
    V(egonet)$type <-
      ifelse(vertex.attributes(egonet)$name %in% antecedents, TRUE, FALSE)
    V(egonet)$color <-
      ifelse(V(egonet)$type, "lightblue", "salmon") # color set if there are no clusters
    V(egonet)$shape <- "italic"

    V(egonet)$type[V(egonet)$name == element] <- "element"
    V(egonet)$color[V(egonet)$name == element] <-
      "black" # color set if there are no clusters
    V(egonet)$shape[V(egonet)$name == element] <- "bold"
    V(egonet)$shape[V(egonet)$name %in% antecedents &
      V(egonet)$name %in% consequents] <- "bold"
    V(egonet)$color[V(egonet)$name %in% antecedents &
      V(egonet)$name %in% consequents] <- "orange"

    for (i in 1:length(vertex.attributes(egonet)$size)) {
      vertex.attributes(egonet)$size[i] <-
        unique(transitions$count.antecedent[transitions$antecedent == vertex.attributes(egonet)$name[i]])
    }

    # make graph
    p <- ggraph(
      graph = egonet,
      layout = "igraph",
      algorithm = "sugiyama"
    ) + # algorithm could be changed, e.g. to 'graphopt'
      geom_edge_link(
        mapping = aes(label = round(.data$observed.prob, 2)),
        # this creates and changes the edges
        arrow = arrow(length = unit(5, "mm")),
        colour = "black",
        end_cap = circle(10, "mm"),
        start_cap = circle(10, "mm"),
        label_dodge = unit(3, "mm"),
        angle_calc = "along",
        show.legend = FALSE
      ) +
      geom_edge_loop(
        mapping = aes(
          label = round(.data$observed.prob, 2),
          span = 20,
          strength = 0.5
        ),
        # this creates and changes the edges
        arrow = NULL,
        colour = "black",
        end_cap = circle(3, "mm"),
        start_cap = circle(2, "mm"),
        label_dodge = unit(3, "mm"),
        angle_calc = "along",
        show.legend = FALSE
      ) +
      geom_node_label(
        mapping = aes(
          color = .data$color,
          label = .data$name,
          size = .data$size,
          fontface = .data$shape
        ),
        show.legend = FALSE
      ) +
      coord_fixed() +
      scale_size_continuous(range = c(3, 7), breaks = c(1:100)) +
      ggtitle(element) +
      theme_graph(base_family = "sans")

    return(list(plot = p))
  }
