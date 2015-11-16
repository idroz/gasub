# gasub
R package for extraction of active subnetworks using a global search algorithm

## Installation
```
library(devtools)
install_git("idroz/gasub")
```

## Usage
```
library(gasub)

g <- graph.lattice(dimvector = c(5,6))
weights <- runif(n = vcount(g), min = 0, max = 1)
V(g)$weights <- weights
V(g)$name <- as.character(1:vcount(g))

seed <- V(g)$weights
names(seed) <- V(g)$name

ga <- extract_subgraph(g, seed, parallel = 4)
```
