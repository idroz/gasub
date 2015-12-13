.check_input_params <- function(graph, v){
  # Simple error checking routines for input variables

  # Ensure that graph is an igraph object
  if( class(graph)!="igraph" ){
    stop("Input graph must be an igraph object")
  }

  # Ensure that V is a named vector
  if( !is.vector(v) ){
    stop("v must be a named vector")
  }

  # Check that there is at least 1 seed node in the input graph
  if( sum(names(v) %in% igraph::V(graph)$name)==0 ){
    stop("There are no seed vertices in the graph object")
  }
}
