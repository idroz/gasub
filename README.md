# gasub
R package for extraction of active subnetworks using a global search algorithm

## Installation
```
library(devtools)
install_git("https://github.com/idroz/gasub")
```

## Usage
```
library(gasub)
library(igraph)

g <- graph.lattice(dimvector = c(5,6))
V(g)$name <- seq_len(vcount(g))
weights <- runif(n = vcount(g), min = 0, max = 1)

seed <- weights
names(seed) <- V(g)$name

ga <- gaSubgraph(g, weights = seed, parallel = 4)

ggnet(ga)

```
