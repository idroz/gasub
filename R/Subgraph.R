#' Use genetic algorithm to identify maximally scoring subgraph
#' @param graph       a named igraph object
#' @param weights     a named numeric vector
#' @param pop.size    integer
#' @param max.iter    integer
#' @param run         integer
#' @param p.mutation  float. Probability of mutation
#' @param eletism     integer
#' @param ncores      integer
#'
#' @importFrom igraph get.edgelist vcount
#' @importFrom jsonlite toJSON
#' @export

Subgraph <- function(graph, weights, pop.size = 50, max.iter = 100, run = vcount(graph),
                    p.mutation = 0.1, eletism = max(1, round(pop.size * 0.05)), ncores){

  # Error checking
  if (length(weights) != vcount(graph)) stop("length(weights) must be equal to number of graph nodes")
  if (p.mutation < 0 | p.mutation > 1) stop("Mutation probability must be 0-1")

  if (is.null (V(graph)$name)) stop("graph object must have a non-empthy V(graph)$name attribute")
  if (is.null (names(weights))) stop("weights must be a named vector")

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
  options$ncores <- ncores


  optionsfile <- paste0(getwd(), "/opts.json")
  popfile <- paste0(getwd(), "/population.csv")
  fitnessfile <- paste0(getwd(), "/fitness.csv")

  fileConn<-file(optionsfile)
  writeLines(toJSON(options, auto_unbox = TRUE), fileConn)
  close(fileConn)

  Genetic <- system.file("julia", "Subgraph.jl", package = "gasub")

  system(paste0("julia -p", ncores," ", Genetic, " ", optionsfile, " ", popfile, " ", fitnessfile))

  pop <- read.csv("population.csv", header = FALSE)
  fitness <- as.matrix(read.csv("fitness.csv", header = FALSE))

  unlink(optionsfile)
  unlink(popfile)
  unlink(fitnessfile)

  res <- structure(list(population = pop, fitness = fitness), class = "GA")
  return(res)

}
