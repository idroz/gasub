.check_input_params <- function(graph, seed){
  # Simple error checking routines for input variables

  # Ensure that graph is an igraph object
  if( class(graph)!="igraph" ){
    stop("Input graph must be an igraph object")
  }

  # Ensure that seed is a named vector
  if( !is.vector(seed) ){
    stop("seed must be a named vector")
  }

  # Check that there is at least 1 seed node in the input graph
  if( sum(names(seed) %in% igraph::V(graph)$name)==0 ){
    stop("There are no seed vertices in the graph object")
  }
}


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

extract_subgraph <- function(graph, seed, run = 50, maxiter = 100000, parallel = 2, ...){
  .check_input_params(graph, seed)

  #Replace absent seed nodes with 0
  full_seed = rep(0, igraph::vcount(graph))
  names(full_seed) <- igraph::V(graph)$name
  full_seed[match(names(seed), igraph::V(graph)$name)] <- seed

  GA <- ga(type = "binary", fitness = .fitness, G = graph, W = full_seed, nBits = igraph::vcount(graph), seed = 123, run = run, maxiter = maxiter, parallel = parallel, ...)
  sub.g <- igraph::induced_subgraph(graph, which(GA@solution[1,]==1))

  return(sub.g)
}
