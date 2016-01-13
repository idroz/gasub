#' Compute network precision
#'
#' @param predicted igraph object
#' @param actual    igraph object
#' @export

graphPrecision <- function(predicted, actual){
  igraph::vcount(igraph::graph.intersection(actual, predicted, keep.all.vertices = FALSE))/igraph::vcount(igraph::graph.union(actual, predicted))
}

#' Compute network recall
#'
#' @param predicted igraph object
#' @param actual    igraph object
#' @export

graphRecall <- function(predicted, actual){
  igraph::vcount(igraph::graph.intersection(actual, predicted, keep.all.vertices = FALSE))/igraph::vcount(actual)
}
