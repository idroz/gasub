#' Compute network precision
#'
#' @param predicted igraph object
#' @param actual    igraph object
#' @export

graphPrecision <- function(predicted, actual){

  tp <- igraph::ecount(igraph::graph.intersection(actual, predicted, keep.all.vertices = FALSE))
  fp <- igraph::ecount(igraph::graph.difference(predicted, actual))

  tp/(tp + fp)
}

#' Compute network recall
#'
#' @param predicted igraph object
#' @param actual    igraph object
#' @export

graphRecall <- function(predicted, actual){
  tp <- igraph::ecount(igraph::graph.intersection(actual, predicted, keep.all.vertices = FALSE))
  fn <- igraph::ecount(igraph::graph.difference(actual, predicted))

  tp/(tp + fn)
}
