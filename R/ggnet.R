#' Plot a network using ggplot2
#'
#' @param g igraph object
#' @param node.lbl character vector of graph node labels
#' @param node.size vector of integers proportional to the size of the node
#' @param node.col vector of intergers proportional to the colour of the node
#' @param node.alpha integer [0, 1] corresponding to node transparancy
#' @param low.col colour of the lowest value
#' @param high.col colour of the highest value
#'
#' @export

ggnet <- function(g, node.lbl = NA, node.size = NA, node.col = NA, node.alpha = 0.5, low.col = "blue", high.col = "red", edge.width, edge.col){

  # Check input class
  if( !class(g)=="igraph" ) stop("Input must be an igraph object")

  # Ensure the graph has $name attribute
  if( is.na(node.lbl) ) node.lbl <- as.character(seq_len(igraph::vcount(g)))

  # Handle missing node colour values
  if( is.na(node.col)) node.col <- rep(1, igraph::vcount(g))

  if (length(node.col) != igraph::vcount(g)) stop("length(node.col) must equal the number of vertices in g")

  xy <- as.data.frame(igraph::layout_nicely(g, dim = 2))

  # Handle missing node size values
  if(any(is.na(node.size))) node.size <- rep((max(xy) - min(xy))/igraph::vcount(g), igraph::vcount(g))


  xy$name <- node.lbl
  xy$node.size <- node.size
  xy$node.col <- node.col

  res <- igraph::get.data.frame(g)

  res$from.x <- xy$V1[match(res$from, xy$name)]
  res$from.y <- xy$V2[match(res$from, xy$name)]
  res$to.x <- xy$V1[match(res$to, xy$name)]
  res$to.y <- xy$V2[match(res$to, xy$name)]

  gnet <- ggplot2::ggplot() +
          ggplot2::geom_text(data = xy, )
          ggplot2::geom_segment(data = res, ggplot2::aes(x=from.x,xend = to.x, y=from.y,yend = to.y)) +
          ggplot2::geom_point(data = xy, alpha = node.alpha, ggplot2::aes(x = V1, y = V2, size = node.size, colour = node.col)) +
          ggplot2::scale_colour_gradient(limits=c(min(node.col), max(node.col)), low=low.col, high = high.col) +
          ggplot2::xlab("") + ggplot2::ylab("") +
          ggplot2::theme(legend.position="none", panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank())

  return(gnet)
}
