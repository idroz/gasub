# Fitness functions that will can maximised by the genetic algorithm

# Node-neighborhood weight maximisation
.weight_fitness <- function(sub, G, W){
  igraph::V(G)$W <- W
  sub.index <- which(sub == 1)
  sub.g <- igraph::induced_subgraph(G, sub.index)

  sum(igraph::V(sub.g)$W * unlist(lapply(igraph::neighborhood(sub.g, 1), .get_neighborhood_weights, weight = igraph::V(sub.g)$W))) / sqrt(igraph::vcount(sub.g))
}

.get_neighborhood_weights <- function(x, weight){
  # Scoring Functions for Subgraph extraction
  return(sum(weight[x]))
}

# Network modularity maximisation
.modularity_fitness <- function(sub, G, W){
  igraph::V(G)$W <- W
  sub.index <- which(sub == 1)
  sub.g <- igraph::induced_subgraph(G, sub.index)

  subgraph.clusters <- igraph::fastgreedy.community(sub.g)
  subgraph.weights <- .get_module_weights(subgraph.clusters, W)
  mod <- igraph::modularity(sub.g, subgraph.clusters$membership)
  sum(mod * subgraph.weights)
}

.get_module_weights <- function(clusters, W){
  unlist(lapply(igraph::groups(clusters), function(x) mean(W[names(W) %in% x])))
}
