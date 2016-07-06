#' Extract an active subnetwork using Genetic Algorithm scoring function optimisation
#'
#' @param graph             input igraph object
#' @param weights           named vector of node weights. Vector names must match igraph node
#'                          NAME attribute. In an input graph, nodes not found in V will be
#'                          assigned a weight of 0.
#' @param activity.fun  s   coring function to be maximised
#' @param run               the number of consecutive generations without any improvement
#'                          in the best finess value before the GA is stopped
#' @param init.threshold    initial weight threshold above which weights are significant.
#'                          optional parameter that speeds up training. If omitted, algorithm
#'                          still converges accurately, albeit slower.
#' @param maxiter           the maximum number of iterations to run before the GA search
#'                          is halted.
#' @param parallel          logical argument specifying if parallel computing should be
#'                          used (‘TRUE’) or not (‘FALSE’, default) for evaluating the
#'                          fitness function. This argument could also be used to specify
#'                          the number of cores to employ; by default, this is taken from
#'                          ‘detectCores’. Finally, the functionality of parallelization
#'                          depends on system OS: on Windows only 'snow' type
#'                          functionality is available, while on Unix/Linux/Mac OSX both
#'                          'snow' and 'multicore' (default) functionalities are
#'                          available.
#' @param ...               additional arguments to be passed to the ga() function
#' @export

gaSubgraph <- function(graph, weights, activity.fun = .weight_fitness, run = 50, init.threshold = NULL, maxiter = 100000, parallel = 2, ...){
  .check_input_params(graph, weights)

  # Only keep seed nodes that are present in the graph
  weights <- weights[ names(v) %in% igraph::V(graph)$name ]

  # Assign a weight 0 to all nodes in the graph.
  # This ensures that nodes without an assigned weight have a default weight of 0
  full.seed = rep(0, igraph::vcount(graph))
  names(full.seed) <- igraph::V(graph)$name

  # Find locations of input seed nodes in the graph
  ix <- match(names(weights), igraph::V(graph)$name)
  # Remove NA indeces
  ix <- ix[!is.na(ix)]

  full.seed[ix] <- weights

  suggestions <- NULL
  if(!is.null(init.threshold)) suggestions <- as.numeric(full.seed >= init.threshold)

  GA <- ga(fitness = activity.fun, G = graph, W = full.seed, nBits = igraph::vcount(graph), seed = 123, run = run, maxiter = maxiter, parallel = parallel, suggestions = suggestions)

  sub.g <- igraph::induced_subgraph(graph, which(GA@solution[1,]==1))

  return(sub.g)
}
