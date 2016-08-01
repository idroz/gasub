#' Extract an active subnetwork using Genetic Algorithm scoring function optimisation
#'
#' @param graph             input igraph object
#' @param weights           named vector of node weights. Vector names must match igraph node
#'                          NAME attribute. In an input graph, nodes not found in V will be
#'                          assigned a weight of 0.
#' @param run               the number of consecutive generations without any improvement
#'                          in the best finess value before the GA is stopped
#' @param max.iter           the maximum number of iterations to run before the GA search
#'                          is halted.
#' @export

gaSubgraph <- function(graph, weights, ...){

  # Only keep seed nodes that are present in the graph
  weights <- weights[ names(weights) %in% V(graph)$name ]

  # Assign a weight 0 to all nodes in the graph.
  # This ensures that nodes without an assigned weight have a default weight of 0
  full.seed <- rep(0, vcount(graph))
  names(full.seed) <- V(graph)$name

  # Find locations of input seed nodes in the graph
  ix <- match(names(weights), V(graph)$name)
  # Remove NA indeces
  ix <- ix[!is.na(ix)]

  full.seed[ix] <- weights



  GA <- ga(fitness, G = graph, W = full.seed, n.bits = vcount(graph), ...)

  sub.g <- induced_subgraph(graph, which(GA$population[which.max(GA$fitness), ] == 1))


  return(sub.g)
}
