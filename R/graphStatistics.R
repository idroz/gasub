#' Compute network precision
#'
#' @param predicted igraph object
#' @param actual    igraph object
#' @export

graphPrecision <- function(predicted, actual){
  igraph::vcount(igraph::graph.intersection(actual, predicted))/igraph::vcount(igraph::graph.union(actual, predicted))
}

#' Compute network recall
#'
#' @param predicted igraph object
#' @param actual    igraph object
#' @export

graphRecall <- function(predicted, actual){
  igraph::vcount(igraph::graph.intersection(actual, subg))/igraph::vcount(actual)
}
