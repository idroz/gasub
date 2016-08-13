# gasub
R package for extraction of active subnetworks using a global search algorithm

## Installation

The package has been tested on OSX and Unix systems.

### Installing [Julia](http://julialang.org/downloads/)
Computationally intensive tasks of GASUB are implemented in Julia. Currently, the package requires Julia v0.4.6

Additionally, the following Julia packages are required:

```
Pkg.add("Distributions")
Pkg.add("JSON")
Pkg.add("LightGraphs")

Pkg.update()
```

### Installing the R package

```
devtools::install_git("https://github.com/idroz/gasub")
```

## Usage
```
library(gasub)
library(igraph)

g <- graph.lattice(dimvector = c(5,6))
V(g)$name <- seq_len(vcount(g))
weights <- runif(n = vcount(g), min = 0, max = 1)
names(weights) <- V(g)$name

ga <- Subgraph(g, weights)

subgraph <- delete.vertices(g, which(ga$population == 0))

plot(subgraph)
```
