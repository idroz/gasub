library(igraph)
library(ga)
# Scoring Functions for Subgraph extraction #####
get.neighborhood.weights <- function(x, weight){
  return(sum(weight[x]))
}

fitness <- function(sub, G, W){
  V(G)$W <- W
  sub.index <- which(sub == 1)
  sub.g <- induced.subgraph(G, sub.index)

  sum(V(sub.g)$W * unlist(lapply(neighborhood(sub.g, 1), get.neighborhood.weights, weight = V(sub.g)$W))) / sqrt(vcount(sub.g))
}

extract.subgraph <- function(ppi, seed){

  GA <- ga(type = "binary", fitness = fitness, G = ppi, W = seed, nBits = vcount(ppi), seed = 123, run = 50, maxiter = 100000, parallel = 4)
  sub.g <- induced.subgraph(ppi, which(GA@solution[1,]==1))

  return(sub.g)
}
