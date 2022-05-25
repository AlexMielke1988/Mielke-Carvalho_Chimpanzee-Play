#' Create network plot for element connections - plots the significant antecedents and consequents for all element
#'
#' @param elem.bout list of vectors of elements. Co-occurring elements are marked by the percentage symbol.
#' @param elem.time list of vectors of timings of elements. If set to NULL, the order of elements is used with distance of 1
#' @param edge.weight either 'transition', in which case the edges are defined by the probability of A transitioning to B, or 'bag', in which case a 'bag-of-words' approach is chosen
#' @param link determines how nodes/elements are connected. 'unweighted' gives a 1 to significant connections and 0 to all others; 'weighted' gives the difference between observed and expected probability of co-occurrence; 'raw' just uses the observed probability of co-occurrence; 'SRI' uses the simple ratio index/affinity (probability of co-occurrence/ (probabilities of each element and the combination))
#' @param min.count numeric value, suggesting how many times a combination should at least occur to be displayed
#' @param min.prob numeric value, suggesting the probability at which a combination should at least occur to be displayed#'
#' @param significance level of significance for transitions to be considered significantly more common than expected; default is sig = 0.01
#' @param hide_unconnected if TRUE, then the nodes that do not have any significant connections will be hidden in the plot
#' @param plot.bubbles if TRUE, then the nodes in the network plots will be surrounded by bubbles; if FALSE, the edges connect the names directly
#' @param clusters should clusters be calculated and added?
#' @param ignore.element vector of elements that will not be considered for the network, e.g. because they are too common or too rare or their interpretation is not relevant here
#' @param title string, will be the title of the resulting plot
#' @param facet if TRUE, plot will be split by clusters; default is FALSE
#' @param remove_loops if FALSE, significant connections of elements with themselves are shown; default is FALSE
#' @param gap number of elements to each side that should be counted in the same 'bag' as the element; if 'all', use whole bout
#' @param it how many iterations of the random splitting of co-occurring elements should be used? If elements never co-occur, this can be set to 1. If there are many co-occurrences, this number should be high, but this will make things slower
#' @param cores number of cores for parallel execution
#' @param plot.layout one of the igraph layout options; works okay with 'fr', 'lgl', 'dh', 'nicely', 'kk', or 'graphopt', depending on the number of elements
#' @param ran.method for co-occurring elements, how should they be split? Can be 'random' (order is randomised), or 'sample' (randomly choose any)
#'
#' @return Returns a ggplot element indicating the antecedents, descendents, and probabilities in each direction; also returns an igraph graph and the centrality measures for each element
#'
#' @importFrom dplyr mutate arrange desc as_tibble select all_of na_if left_join bind_rows bind_cols rename mutate_all
#' @importFrom stringr str_detect str_split str_replace_all str_to_title str_to_lower
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na gather separate unite
#' @importFrom ggplot2 ggplot geom_label aes ggtitle xlab ylab theme_classic annotate geom_hline coord_fixed scale_size_continuous arrow unit scale_size
#' @importFrom ggraph ggraph create_layout geom_node_text scale_edge_alpha theme_graph geom_edge_fan geom_node_label geom_edge_link geom_edge_loop circle facet_nodes geom_node_point
#' @importFrom igraph vertex.attributes vertex.attributes<- add_vertices V edge.attributes edge.attributes<- graph_from_data_frame graph.adjacency delete_edges add_vertices get.data.frame bipartite_mapping cluster_fast_greedy modularity V<- graph_from_edgelist betweenness transitivity hub_score page_rank eigen_centrality strength walktrap.community delete_vertices cluster_optimal
#' @importFrom grDevices rainbow
#' @importFrom graphics layout
#' @importFrom stats complete.cases
#' @importFrom rlang .data
#'
#' @author Alex Mielke
#' @export
#'
#' @examples
#' ###
network_plot <- function(elem.bout,
                         elem.time = NULL,
                         edge.weight = "transition",
                         min.prob = 0.2,
                         min.count = 5,
                         significance = 0.05,
                         hide_unconnected = T,
                         link = "unweighted",
                         clusters = T,
                         plot.bubbles = FALSE,
                         ignore.element = NULL,
                         title = "network",
                         facet = FALSE,
                         remove_loops = FALSE,
                         gap = c(-2, 2),
                         it = 1,
                         cores = 5,
                         plot.layout = "fr",
                         ran.method = "random") {
  # if edge.weight is 'transition', calculate transition probabilities
  if (edge.weight == "transition") {
    compare.mat <- transitions_frame(
      elem.bout = elem.bout,
      elements = NULL
    )

    # probability of A leading to B
    compare.mat$observed.probs <- elem_info(
      antecedent = compare.mat$antecedent,
      consequent = compare.mat$consequent,
      elem.bout = elem.bout,
      it = it,
      measure = c("prob"),
      ran.method = ran.method
    )

    compare.mat$observed.sum <- elem_info(
      antecedent = compare.mat$antecedent,
      consequent = compare.mat$consequent,
      elem.bout = elem.bout,
      it = it,
      measure = c("sum"),
      ran.method = ran.method
    )

    ############# Randomize - put all elements together, shuffle, put them back into bouts

    randomizations <- randomized_elem_info(
      elem.bout = elem.bout,
      antecedent = compare.mat$antecedent,
      consequent = compare.mat$consequent,
      observed = compare.mat$observed.probs,
      it = it,
      cores = cores,
      trials = it,
      output = "expected",
      ran.method = ran.method
    )

    compare.mat$expected.probs <- randomizations$prob
    compare.mat$pvalue <- randomizations$pvalue
    compare.mat$z <- randomizations$z
    compare.mat$prob.increase <-
      compare.mat$observed.probs / compare.mat$expected.probs
  }

  # if edge.weight is 'bag', use bag_of_words approach
  if (edge.weight == "bag") {
    compare.mat <- bag_of_words(
      elem.bout = elem.bout,
      elem.time = elem.time,
      gap = gap
    )

    ############# Randomize - put all elements together, shuffle, put them back into bouts

    randomizations <- randomized_bag_of_words(
      elem.bout = elem.bout,
      elem.time = elem.time,
      observed = compare.mat$observed.probs,
      gap = gap,
      cores = cores,
      trials = it,
      output = "expected"
    )

    compare.mat$expected.probs <- randomizations$prob
    compare.mat$pvalue <- randomizations$pvalue
    compare.mat$z <- randomizations$z
    compare.mat$prob.increase <-
      compare.mat$observed.probs / compare.mat$expected.probs
    compare.mat$antecedent <- compare.mat$element
    compare.mat$consequent <- compare.mat$other
    compare.mat$prob.antecedent <-
      compare.mat$count.element / sum(compare.mat$count.element[!duplicated(compare.mat$element)])
  }

  # as node weight for the network, set probability of each antecedent
  node.weight <- compare.mat[!duplicated(compare.mat$antecedent), ]
  trans.mat <- compare.mat

  # prepare data by removing all combinations that do not meet probability and significance standards
  compare.mat <-
    compare.mat[compare.mat$observed.probs >= min.prob &
      compare.mat$observed.sum >= min.count &
      compare.mat$observed.probs > compare.mat$expected.probs &
      compare.mat$pvalue <= significance, ]

  # remove NAs
  compare.mat <- compare.mat[complete.cases(compare.mat), ]

  # decide whether to include loops or not
  if (remove_loops) {
    compare.mat <-
      compare.mat[compare.mat$antecedent != compare.mat$consequent, ]
  }
  # set element1 and element2
  compare.mat$element1 <- compare.mat$antecedent
  compare.mat$element2 <- compare.mat$consequent

  # if any elements were specified to be ignored, set those
  compare.mat <-
    compare.mat[!(compare.mat$element1 %in% ignore.element) &
      !(compare.mat$element2 %in% ignore.element), ]

  # remove uninteresting columns
  compare.mat <- compare.mat[, c(
    "element1",
    "element2",
    "observed.probs",
    "expected.probs",
    "pvalue",
    "z",
    "observed.sum"
  )]

  # create graph object
  descriptive.graph <-
    graph_from_data_frame(compare.mat,
      directed = T,
      vertices = NULL
    )
  # set node size
  vertex.attributes(descriptive.graph)$element.prob <-
    node.weight$prob.antecedent[match(
      vertex.attributes(descriptive.graph)$name,
      node.weight$antecedent
    )]

  # set edge weight based on whether it's weighted or not
  if (link == "unweighted") {
    edge.attributes(descriptive.graph)$unweighted <-
      (edge.attributes(descriptive.graph)$pvalue <= significance)
    descriptive.graph <-
      delete_edges(descriptive.graph, edges = which(!edge.attributes(descriptive.graph)$unweighted))
    edge.attributes(descriptive.graph)$weight <-
      as.numeric(edge.attributes(descriptive.graph)$unweighted)
    edge.attributes(descriptive.graph)$association <-
      edge.attributes(descriptive.graph)$observed.prob - edge.attributes(descriptive.graph)$expected.prob
  }
  if (link == "weighted") {
    edge.attributes(descriptive.graph)$weight <-
      edge.attributes(descriptive.graph)$observed.probs - edge.attributes(descriptive.graph)$expected.prob
  }
  if (link == "raw") {
    edge.attributes(descriptive.graph)$weight <-
      edge.attributes(descriptive.graph)$observed.probs
  }

  # which nodes did not have any connections?
  missing.nodes <-
    setdiff(node.weight$antecedent, V(descriptive.graph)$name)
  missing.nodes <- setdiff(missing.nodes, ignore.element)
  # add missing nodes to graph
  descriptive.graph <- add_vertices(descriptive.graph,
    length(missing.nodes),
    attr = list(name = missing.nodes)
  )


  net.graph <- descriptive.graph

  # should nodes without connections be seen or hidden?
  if (hide_unconnected == T) {
    unused <- which(
      vertex.attributes(net.graph)$element.prob == 0 |
        is.na(vertex.attributes(net.graph)$element.prob)
    )
    net.graph <- delete_vertices(net.graph, unused)
  }

  # prepare node and edge information
  node.label <-
    vertex.attributes(net.graph)$name # nodes are named as they were in the original network object
  node.size <-
    vertex.attributes(net.graph)$element.prob # size of nodes is determined by their probability to occur
  node.size[is.na(node.size)] <- min(node.size, na.rm = TRUE)

  edge.weight <-
    edge.attributes(net.graph)$weight # weight of edges is determined by their weight attribute
  edge.size <-
    cut(edge.weight, 3) # the line width of the edges is assinged to either weak, medium, strong
  edge.size.char <- as.character(edge.size)
  edge.size.char[edge.size == levels(edge.size)[1]] <- 1
  edge.size.char[edge.size == levels(edge.size)[2]] <- 3
  edge.size.char[edge.size == levels(edge.size)[3]] <- 5
  edge.size <- as.numeric(edge.size.char)
  if (length(unique(edge.size)) == 1) {
    edge.size <- edge.size / edge.size
  }

  # if 'cluster' is not selected, the graph is plotted in black and white
  if (!clusters) {
    p <- ggraph(
      graph = net.graph,
      layout = "igraph",
      algorithm = plot.layout
    ) + # algorithm could be changed, e.g. to 'graphopt'
      geom_edge_link(
        # this creates and changes the edges
        arrow = arrow(length = unit(2, "mm")),
        end_cap = circle(3, "mm"),
        colour = "grey",
        start_cap = circle(3, "mm"),
        label_dodge = unit(3, "mm"),
        angle_calc = "along",
        show.legend = FALSE
      ) +
      geom_edge_loop(
        mapping = aes(span = 20, strength = 0.8),
        # this creates and changes the edges
        arrow = NULL,
        colour = "grey",
        end_cap = circle(3, "mm"),
        start_cap = circle(2, "mm"),
        label_dodge = unit(3, "mm"),
        angle_calc = "along",
        show.legend = FALSE
      ) +
      geom_node_text(
        mapping = aes(
          label = .data$name,
          # this creates and changes the nodes
          size = node.size,
          fontface = "bold"
        ),
        show.legend = FALSE
      ) +
      scale_size(range = c(4, 6)) +
      ggtitle(title) +
      theme_graph(base_family = "sans")

    if (plot.bubbles == T) {
      p <- p +
        geom_node_point(
          mapping = aes(size = 10, alpha = 0.01),
          color = color[net.com$community],
          shape = shape[net.com$community],
          show.legend = F
        ) +
        coord_fixed() +
        scale_size(range = c(0.5, 10)) +
        geom_node_text(
          mapping = aes(
            label = .data$name,
            size = 0.5,
            fontface = "bold"
          ),
          show.legend = FALSE
        )
    }
  }

  # if 'clusters' == T, then the fast and greedy algorithm is used to detect clusters and color the nodes accordingly
  if (clusters) {
    net.un <- net.graph
    net.community <-
      cluster_optimal(net.un) # other clustering algorithms exist, eg walktrap
    modular <-
      round(modularity(net.community), 2) # modularity measure. Above 0.3 is good modularity
    net.com <- data.frame(
      element = net.community$names,
      community = net.community$membership
    )
    color <- rainbow(length(unique(net.com$community)))
    shape <- sample(rep(15:19, 10))
    vertex.attributes(net.graph)$community <- net.com$community

    p <- ggraph(
      graph = net.graph,
      # see above
      layout = "igraph",
      algorithm = plot.layout
    ) +
      geom_edge_link(
        arrow = arrow(length = unit(2, "mm")),
        colour = "grey",
        end_cap = circle(3, "mm"),
        start_cap = circle(2, "mm"),
        label_dodge = unit(3, "mm"),
        angle_calc = "along",
        show.legend = FALSE
      ) +
      geom_edge_loop(
        mapping = aes(span = 20, strength = 0.8),
        # this creates and changes the edges
        arrow = NULL,
        colour = "grey",
        end_cap = circle(3, "mm"),
        start_cap = circle(2, "mm"),
        label_dodge = unit(3, "mm"),
        angle_calc = "along",
        show.legend = FALSE
      ) +
      geom_node_text(
        mapping = aes(
          label = .data$name,
          color = color[net.com$community],
          size = node.size,
          fontface = "bold"
        ),
        show.legend = FALSE
      ) +
      scale_size(range = c(4, 4.1)) +
      ggtitle(paste(c(title, "; Modularity = ", modular), collapse = "")) +
      theme_graph(base_family = "sans")

    if (plot.bubbles == T) {
      p <- p +
        geom_node_point(
          mapping = aes(size = 10, alpha = 0.01),
          color = color[net.com$community],
          shape = shape[net.com$community],
          show.legend = F
        ) +
        coord_fixed() +
        scale_size(range = c(0.5, 10)) +
        geom_node_text(
          mapping = aes(
            label = .data$name,
            size = 0.5,
            fontface = "bold"
          ),
          show.legend = FALSE
        )
    }
    if (facet) {
      p <- p +
        facet_nodes(~ net.com$community, scales = "free")
    }
  }

  ########## Network measures
  net.measure <- data.frame(
    element = unique(unlist(vertex.attributes(net.graph)$name)),
    strength = 0,
    eigenvector = 0,
    betweenness = 0,
    transitivity = 0,
    hub_score = 0,
    page_rank = 0,
    modularity = 0,
    comm.membership = 0,
    comm.value = 0
  )

  net.strength <- strength(net.graph,
    mode = "total",
    weights = edge.attributes(net.graph)$weight
  )
  net.strength <- net.strength / ((length(net.strength) - 1) * 2)
  net.measure$strength <-
    unlist(lapply(1:nrow(net.measure), function(y) {
      xx <- net.strength[names(net.strength) == net.measure$element[y]]
      if (length(xx) == 1) {
        return(xx)
      }
      if (length(xx) == 0) {
        return(0)
      }
    }))

  net.eigen <-
    eigen_centrality(net.graph, weights = edge.attributes(net.graph)$weight)$vector
  net.measure$eigenvector <-
    unlist(lapply(1:nrow(net.measure), function(y) {
      xx <- net.eigen[names(net.eigen) == net.measure$element[y]]
      if (length(xx) == 1) {
        return(xx)
      }
      if (length(xx) == 0) {
        return(0)
      }
    }))

  net.btw <-
    betweenness(net.graph, weights = edge.attributes(net.graph)$weight)
  net.btw <- net.btw / ((length(net.btw) - 1) * 2)
  net.measure$betweenness <-
    unlist(lapply(1:nrow(net.measure), function(y) {
      xx <- net.btw[names(net.btw) == net.measure$element[y]]
      if (length(xx) == 1) {
        return(xx)
      }
      if (length(xx) == 0) {
        return(0)
      }
    }))

  if (length(unique(edge.attributes(net.graph)$weight)) == 1) {
    type <- "undirected"
  }
  if (length(unique(edge.attributes(net.graph)$weight)) > 1) {
    type <- "weighted"
  }
  net.trans <- transitivity(net.graph,
    type = "local",
    weights = edge.attributes(net.graph)$weight
  )
  names(net.trans) <- V(net.graph)$name
  net.measure$transitivity <-
    unlist(lapply(1:nrow(net.measure), function(y) {
      xx <- net.trans[names(net.trans) == net.measure$element[y]]
      if (length(xx) == 1) {
        return(xx)
      }
      if (length(xx) == 0) {
        return(0)
      }
    }))

  net.hub <-
    hub_score(net.graph, weights = edge.attributes(net.graph)$weight)$vector
  net.measure$hub_score <-
    unlist(lapply(1:nrow(net.measure), function(y) {
      xx <- net.hub[names(net.hub) == net.measure$element[y]]
      if (length(xx) == 1) {
        return(xx)
      }
      if (length(xx) == 0) {
        return(0)
      }
    }))

  net.page <-
    page_rank(net.graph, weights = edge.attributes(net.graph)$weight)$vector
  net.measure$page_rank <-
    unlist(lapply(1:nrow(net.measure), function(y) {
      xx <- net.page[names(net.page) == net.measure$element[y]]
      if (length(xx) == 1) {
        return(xx)
      }
      if (length(xx) == 0) {
        return(0)
      }
    }))

  net.community <- walktrap.community(net.graph)
  net.modularity <- net.community$modularity
  net.measure$modularity <-
    unlist(lapply(1:nrow(net.measure), function(y) {
      xx <- net.modularity[names(net.btw) == net.measure$element[y]]
      if (length(xx) == 1) {
        return(xx)
      }
      if (length(xx) == 0) {
        return(0)
      }
    }))
  net.measure$comm.membership <-
    unlist(lapply(1:nrow(net.measure), function(y) {
      xx <-
        net.community$membership[names(net.btw) == net.measure$element[y]]
      if (length(xx) == 1) {
        return(xx)
      }
      if (length(xx) == 0) {
        return(0)
      }
    }))
  net.measure$comm.value <-
    round(modularity(net.community), 2) # modularity measure. Above 0.3 is good modularity

  net.measure[sapply(net.measure, is.numeric)] <-
    lapply(net.measure[sapply(net.measure, is.numeric)], round, 3)

  clus1 <-
    net.measure$comm.membership[match(trans.mat$antecedent, net.measure$element)]
  clus2 <-
    net.measure$comm.membership[match(trans.mat$consequent, net.measure$element)]

  transitions_clus <-
    sum(trans.mat$observed.sum[clus1 == clus2], na.rm = T) /
      sum(trans.mat$observed.sum[!is.na(clus1 == clus2)])
  dyads_clus <- sum(clus1 == clus2, na.rm = T) / length(clus1)

  return(
    list(
      plot = p,
      network = net.graph,
      centrality = net.measure,
      within_cluster_transitions = c(
        dyads_within = dyads_clus,
        transitions_within = transitions_clus,
        transitions_ratio = transitions_clus / dyads_clus
      )
    )
  )
}
