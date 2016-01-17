# Fitness functions that will can maximised by the genetic algorithm

# Node-neighborhood weight maximisation
.weight_fitness <- function(sub, G, W){
  igraph::V(G)$W <- W
  sub.index <- which(sub == 1)
  sub.g <- igraph::induced_subgraph(G, sub.index)

  neighbor.weights <- unlist(lapply(igraph::neighborhood(sub.g, 1), .get_neighborhood_weights, weight = igraph::V(sub.g)$W))
  sum(igraph::V(sub.g)$W * neighbor.weights) / sqrt(igraph::vcount(sub.g))
}

.get_neighborhood_weights <- function(x, weight){
  # Scoring Functions for Subgraph extraction
  return(sum(weight[x]))
}
