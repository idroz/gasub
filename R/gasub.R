.get_neighborhood_weights <- function(x, weight){
  # Scoring Functions for Subgraph extraction
  return(sum(weight[x]))
}

.fitness <- function(sub, G, W){
  # Fitness function that will be maximised by the genetic algorithm

  igraph::V(G)$W <- W
  sub.index <- which(sub == 1)
  sub.g <- igraph::induced_subgraph(G, sub.index)

  sum(igraph::V(sub.g)$W * unlist(lapply(igraph::neighborhood(sub.g, 1), .get_neighborhood_weights, weight = igraph::V(sub.g)$W))) / sqrt(igraph::vcount(sub.g))
}


#' \code{ga_subgraph}
#'
#' Extract an active subnetwork using the Genetic Algorithm
#'
#' @param graph input igraph object
#' @param v named vector of node weights. Vector names must match igraph node
#'          NAME attribute. In an input graph, nodes not found in V will be
#'          assigned a weight of 0.
#' @param run the number of consecutive generations without any improvement
#'            in the best finess value before the GA is stopped
#' @param maxiter the maximum number of iterations to run before the GA search
#'                is halted.
#' @param parallel a logical argument specifying if parallel computing should be
#'                 used (‘TRUE’) or not (‘FALSE’, default) for evaluating the
#'                 fitness function. This argument could also be used to specify
#'                 the number of cores to employ; by default, this is taken from
#'                 ‘detectCores’. Finally, the functionality of parallelization
#'                 depends on system OS: on Windows only 'snow' type
#'                 functionality is available, while on Unix/Linux/Mac OSX both
#'                 'snow' and 'multicore' (default) functionalities are
#'                  available.
#' @param ... additional arguments to be passed to GA:ga(function)
#' @export

ga_subgraph <- function(graph, v, run = 50, maxiter = 100000, parallel = 2, ...){
  .check_input_params(graph, v)

  #Replace absent seed nodes with 0
  full_seed = rep(0, igraph::vcount(graph))
  names(full_seed) <- igraph::V(graph)$name
  full_seed[match(names(v), igraph::V(graph)$name)] <- v

  GA <- GA::ga(type = "binary", fitness = .fitness, G = graph, W = full_seed, nBits = igraph::vcount(graph), seed = 123, run = run, maxiter = maxiter, parallel = parallel, ...)
  sub.g <- igraph::induced_subgraph(graph, which(GA@solution[1,]==1))

  return(sub.g)
}
