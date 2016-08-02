#' Compute network precision
#'
#' @param predicted igraph object
#' @param actual    igraph object
#' @export

GraphPrecision <- function(predicted, actual){

  tp <- igraph::ecount(igraph::graph.intersection(actual, predicted, keep.all.vertices = FALSE))
  fp <- igraph::ecount(igraph::graph.difference(predicted, actual))

  tp/(tp + fp)
}

#' Compute network recall
#'
#' @param predicted igraph object
#' @param actual    igraph object
#' @export

GraphRecall <- function(predicted, actual){
  tp <- igraph::ecount(igraph::graph.intersection(actual, predicted, keep.all.vertices = FALSE))
  fn <- igraph::ecount(igraph::graph.difference(actual, predicted))

  tp/(tp + fn)
}

#' Compute jaccard similarity coefficient of two networks
#'
#' @param i     igraph object
#' @param j     igraph object
#' @param type  character value - "vertex" or"edge", indicating structures to be compared
#' @export
JaccardSimilarity <- function(i, j, type = "vertex"){
  switch(type,
    "vertex" = {igraph::vcount(igraph::graph.intersection(i,j, keep.all.vertices = FALSE)) / igraph::vcount(igraph::graph.union(i,j))},
    "edge" = {igraph::ecount(igraph::graph.intersection(i,j, keep.all.vertices = FALSE)) / igraph::ecount(igraph::graph.union(i,j))}
    )
}

#' Compute cosine similarity coefficient of two networks
#'
#' @param i     igraph object
#' @param j     igraph object
#' @param type  character value - "vertex" or"edge", indicating structures to be compared
#' @export
CosineSimilarity <- function(i,j, type = "vertex"){
  switch(type,
    "vertex" = {igraph::vcount(igraph::graph.intersection(i,j, keep.all.vertices = FALSE)) / sqrt(igraph::vcount(i) * igraph::vcount(j))},
    "edge" = {igraph::ecount(igraph::graph.intersection(i,j, keep.all.vertices = FALSE)) / sqrt(igraph::ecount(i) * igraph::ecount(j))}
    )
}
