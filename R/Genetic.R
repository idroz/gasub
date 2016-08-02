#' Use genetic algorithm to identify maximally scoring subgraph
#' @param graph       igraph object
#' @param weights     numeric vector
#' @param pop.size    integer
#' @param max.iter    integer
#' @param run         integer
#' @param p.mutation  float. Probability of mutation
#' @param eletism     integer
#'
#' @importFrom igraph get.edgelist
#' @importFrom jsonlite toJSON
#' @export

Genetic <- function(graph, weights, pop.size = 75, max.iter = 1000, run = 20,
                    p.mutation = 0.01, eletism = 1){

  # Generate a json file using input options
  options <- list()
  options$edgelist <- get.edgelist(graph, names = FALSE)
  options$vertexlist <- V(graph)$name
  options$weights <- weights
  options$popsize <- pop.size
  options$pmutation <- p.mutation
  options$eletism <- eletism
  options$maxiter <- max.iter
  options$run <- run


  optionsfile <- paste0(getwd(), "/opts.json")
  popfile <- paste0(getwd(), "/population.csv")
  fitnessfile <- paste0(getwd(), "/fitness.csv")

  fileConn<-file(optionsfile)
  writeLines(toJSON(options, auto_unbox = TRUE), fileConn)
  close(fileConn)

  Genetic <- system.file("julia", "Genetic.jl", package = "gasub")

  system(paste0("julia ", Genetic, " ", optionsfile, " ", popfile, " ", fitnessfile))

  pop <- read.csv("population.csv", header = FALSE)
  fitness <- as.matrix(read.csv("fitness.csv", header = FALSE))

  unlink(optionsfile)
  unlink(popfile)
  unlink(fitnessfile)

  res <- structure(list(population = pop, fitness = fitness), class = "GA")
  return(res)

}
