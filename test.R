library(gasub)
library(igraph)
library(jsonlite)

g <- barabasi.game(n = 100, m = 1, power = 0.1, directed = FALSE)
vertexlist <- as.character(1:vcount(g))
V(g)$name <- vertexlist
weights <- runif(vcount(g))

# Create a target subgraph
c <- fastgreedy.community(g)
idx <- which(membership(c) == 1)
actual.graph <- induced_subgraph(g, idx)
actual.vertexlist <- vertexlist[idx]

# Assign smaller weights to nodes in the target subgraph
weights[idx] <- runif(length(idx), min = 0, max = 0.001)
weights <- -log10(weights)

pop <- Genetic(g, weights, pop.size = 76, eletism = 2)

subg <- induced_subgraph(g, which(pop == 1))
V(subg)$name <- vertexlist[which(pop == 1)]
V(actual.graph)$name <- actual.vertexlist

GraphPrecision(actual.graph, subg)
GraphRecall(actual.graph, subg)
