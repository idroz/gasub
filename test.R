library(gasub)
library(igraph)
library(microbenchmark)

g <- erdos.renyi.game(1000, 1/250)
V(g)$name <- as.character(seq_len(vcount(g)))
clusters <- fastgreedy.community(g)

weight <- runif(n = vcount(g), min = 0, max = 1)
selection <- sample(size = round(0.25 * length(groups(clusters))), x = 1:length(groups(clusters)))
idx <- which(clusters$membership %in% selection)
weight[idx] <- runif(n = length(idx), min = 0, max = 0.05)
weight <- -log10(weight)
names(weight) <- V(g)$name
V(g)$weight <- weight

# Create a gold-standard subgraph
actual <- induced.subgraph(g, idx)

ga <- Subgraph(g, weight, max.iter = 100000, ncores = 2)
subg <- induced.subgraph(g, which(ga$population == 1))

GraphPrecision(actual, subg)
GraphRecall(actual, subg)

#----------Micro benchmarking

nnodes <- c(2000)
mb <- list()

for (i in 1:length(nnodes)){
  print(i)
  g <- erdos.renyi.game(nnodes[i], 1/2000)
  V(g)$name <- as.character(seq_len(vcount(g)))
  clusters <- fastgreedy.community(g)

  weight <- runif(n = vcount(g), min = 0, max = 1)
  selection <- sample(size = round(0.25 * length(groups(clusters))), x = 1:length(groups(clusters)))
  idx <- which(clusters$membership %in% selection)
  weight[idx] <- runif(n = length(idx), min = 0, max = 0.05)
  weight <- -log10(weight)
  names(weight) <- V(g)$name
  V(g)$weight <- weight

  mb[[i]] <- microbenchmark(Subgraph(g, weight, max.iter = 100000, ncores = 1),
                     Subgraph(g, weight, max.iter = 100000, ncores = 2),
                     Subgraph(g, weight, max.iter = 100000, ncores = 3),
                     Subgraph(g, weight, max.iter = 100000, ncores = 4), times = 3)

}

boxplot(mb[[1]])
