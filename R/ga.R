#' Simple implementation of a binary Genetic Algorithm
#' @export
#' @importFrom parallel makeCluster parApply stopCluster
ga <- function(fitness, ..., pop.size = 50, n.bits, max.iter = 20, run = 10, p.mutation = 0.01, eletism = 2, seed = 123){
  set.seed(123)
  cl <- makeCluster(4)
  population <- matrix(rbinom(pop.size * n.bits, 1, 0.5), ncol = n.bits, nrow = pop.size)

  maxscore <- vector()

  for (n in 1:(max.iter + 1)){
    score <- do.call(parApply, args = list(cl = cl, X = population, MARGIN = 1, FUN = fitness, ...))
    ordered <- order(score, decreasing = TRUE)
    elites <- population[ordered[1 : eletism], ]

    maxscore[n] <- max(score)

    # Selection
    prob <- abs(score) / sum(abs(score))
    sel <- sample(1:pop.size, size = pop.size,
                  prob = pmin(pmax(0, prob), 1, na.rm = TRUE),
                  replace = TRUE)
    population <- population[sel, , drop = FALSE]


    # Crossover
    nmating <- floor(pop.size / 2)
    mating <- matrix(sample(1:(2 * nmating), size = (2 * nmating)), ncol = 2)

    Crossover <- apply(mating, 1, function(x) crossover(population[x[1], ], population[x[2], ]))
    population[mating[, 1], ]  <- t(Crossover)

    # Mutation
    idx <- which(runif(max = 1, min = 0, n = pop.size) < p.mutation)
    mutation <- sample(1:n.bits, length(idx))
    population[idx, mutation] <- as.numeric(!(population[idx, mutation]))


    # Eletism
    population[ordered[1:eletism], ] <- elites

    cat("Iteration: ", n - 1, " | Fitness: ", maxscore[n], "\n")

    if (n > run){
      if (length(rle(maxscore[(n - (run - 1)) : n])$values) == 1)
      break
    }
  }

  stopCluster(cl)
  res <- list(population = population, fitness = score, improvement = maxscore)

  return(res)
}


crossover <- function(x, y){
  area <- runif(length(x)) > 0.5
  x[area] <- y[area]
  child <- x
  return(child)
}

#' @importFrom igraph induced_subgraph neighborhood vcount
fitness <- function(sub, G, W){
  igraph::V(G)$W <- W
  sub.index <- which(sub == 1)
  sub.g <- induced_subgraph(G, sub.index)

  neighbor.weights <- unlist(lapply(neighborhood(sub.g, 1), function(x, weight = V(sub.g)$W) sum(weight[x]) ))
  sum(V(sub.g)$W * neighbor.weights) / sqrt(vcount(sub.g))
}
