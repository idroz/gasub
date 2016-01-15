ga <- function(
               fitness, ...,
               min, max, nBits,
               population = gabin_Population,
               selection = gabin_lrSelection,
               crossover = gabin_spCrossover,
               mutation = gabin_raMutation,
               popSize = 50,
               pcrossover = 0.8,
               pmutation = 0.1,
               elitism = base::max(1, round(popSize*0.05)),
               maxiter = 100,
               run = maxiter,
               maxfitness = Inf,
               names = NULL,
               suggestions = NULL,
               keepBest = FALSE,
               parallel = FALSE,
               seed = NULL,
               verbose = TRUE)
{

  eps <- sqrt(.Machine$double.eps)
  if(missing(fitness))
    { stop("A fitness function must be provided") }
  if(!is.function(fitness))
    { stop("A fitness function must be provided") }
  if(popSize < 10)
    { warning("The population size is less than 10.") }
  if(maxiter < 1)
    { stop("The maximum number of iterations must be at least 1.") }
  if(elitism > popSize)
    { stop("The elitism cannot be larger that population size.") }
  if(pcrossover < 0 | pcrossover > 1)
    { stop("Probability of crossover must be between 0 and 1.") }
  if(is.numeric(pmutation))
    { if(pmutation < 0 | pmutation > 1)
        { stop("If numeric probability of mutation must be between 0 and 1.") }
      else if(!is.function(population))
             { stop("pmutation must be a numeric value in (0,1) or a function.") }
  }
  if(missing(min) & missing(max) & missing(nBits))
    { stop("A min and max range of values (for 'real-valued' or 'permutation' GA) or nBits (for 'binary' GA) must be provided!") }

  nBits <- as.vector(nBits)[1]
  min <- max <- NA
  nvars <- nBits

  if(is.null(suggestions))
    { suggestions <- matrix(nrow = 0, ncol = nvars) }
  else
    { if(is.vector(suggestions))
        { if(nvars > 1) suggestions <- matrix(suggestions, nrow = 1)
          else          suggestions <- matrix(suggestions, ncol = 1) }
      else
        { suggestions <- as.matrix(suggestions) }
      if(nvars != ncol(suggestions))
        stop("Provided suggestions (ncol) matrix do not match number of variables of the problem!")
    }

  # Start parallel computing (if needed)
  parallel <- if(is.logical(parallel))
                { if(parallel) startParallel(parallel) else FALSE }
              else { startParallel(parallel) }
  on.exit(if(parallel)
          parallel::stopCluster(attr(parallel, "cluster")) )

  fitnessSummary <- matrix(as.double(NA), nrow = maxiter, ncol = 6)
  colnames(fitnessSummary) <- names(gaSummary(rnorm(10)))
  bestSol <- if(keepBest) vector(mode = "list", length = maxiter)
             else         list()
  Fitness <- rep(NA, popSize)

  object <- new("ga",
                min = min,
                max = max,
                nBits = nBits,
                names = if(is.null(names)) character() else names,
                popSize = popSize,
                iter = 0,
                run = 1,
                maxiter = maxiter,
                suggestions = suggestions,
                population = matrix(),
                elitism = elitism,
                pcrossover = pcrossover,
                pmutation = if(is.numeric(pmutation)) pmutation else NA,
                fitness = Fitness,
                summary = fitnessSummary,
                bestSol = bestSol)

  if(!is.null(seed)) set.seed(seed)

  # generate beginning population
  Pop <- matrix(as.double(NA), nrow = popSize, ncol = nvars)
  ng <- min(nrow(suggestions), popSize)
  if(ng > 0) # use suggestion if provided
    { Pop[1:ng,] <- suggestions }
  # fill the rest with a random population
  if(popSize > ng)
    { Pop[(ng+1):popSize,] <- population(object)[1:(popSize-ng),] }
  object@population <- Pop

  # start iterations
  for(iter in seq_len(maxiter))
     {
      # evalute fitness function (when needed)
      if(!parallel)
        { for(i in seq_len(popSize))
             if(is.na(Fitness[i]))
               { Fitness[i] <- fitness(Pop[i,], ...) }
      }
      else
        { Fitness <- foreach(i = seq_len(popSize), .combine = "c") %dopar%
                     { if(is.na(Fitness[i])) fitness(Pop[i,], ...)
                       else                  Fitness[i] }
        }

      fitnessSummary[iter,] <- gaSummary(Fitness)

      # update object
      object@iter <- iter
      object@population <- Pop
      object@fitness <- Fitness
      object@summary <- fitnessSummary

      if(keepBest)
        # object@bestSol[[iter]] <- unique(Pop[Fitness == bestEval[iter],,drop=FALSE])
        object@bestSol[[iter]] <- unique(Pop[Fitness == fitnessSummary[iter,1],,drop=FALSE])

      if(verbose & ((iter %% 10)==0))
          { gaMonitor(object) }

      # check stopping criteria
      if(iter > 1)
        { if(fitnessSummary[iter,1] > fitnessSummary[iter-1,1]+eps)
               object@run <- 1
          else
               object@run <- object@run + 1
        }
      if(object@run >= run) break
      if(max(Fitness, na.rm = TRUE) >= maxfitness) break
      if(object@iter == maxiter) break

      # PopNew <- matrix(as.double(NA), nrow = popSize, ncol = nvars)
      # fitnessNew <- rep(NA, popSize)
      ord <- order(Fitness, decreasing = TRUE)
      PopSorted <- Pop[ord,,drop=FALSE]
      FitnessSorted <- Fitness[ord]

      # selection
      if(is.function(selection))
        { sel <- selection(object)
          Pop <- sel$population
          Fitness <- sel$fitness
        }
      else
        { sel <- sample(1:popSize, size = popSize, replace = TRUE)
          Pop <- object@population[sel,]
          Fitness <- object@fitness[sel]
        }
      object@population <- Pop
      object@fitness <- Fitness

      # crossover
      if(is.function(crossover) & pcrossover > 0)
        {
          nmating <- floor(popSize/2)
          mating <- matrix(sample(1:(2*nmating), size = (2*nmating)), ncol = 2)
          for(i in seq_len(nmating))
            { if(pcrossover > runif(1))
                { parents <- mating[i,]
                  Crossover <- crossover(object, parents)
                  Pop[parents,] <- Crossover$children
                  Fitness[parents] <- Crossover$fitness
                }
            }
          object@population <- Pop
          object@fitness <- Fitness
        }

      # mutation
      pm <- if(is.function(pmutation)) pmutation(object) else pmutation
      if(is.function(mutation) & pm > 0)
        { for(i in seq_len(popSize))
             { if(pm > runif(1))
                 { Mutation <- mutation(object, i)
                   Pop[i,] <- Mutation
                   Fitness[i] <- NA
                 }
             }
          object@population <- Pop
          object@fitness <- Fitness
        }

      # elitism
      if(elitism > 0)
        { ord <- order(object@fitness, na.last = TRUE)
          u <- which(!duplicated(PopSorted, margin = 1))
          Pop[ord[1:elitism],] <- PopSorted[u[1:elitism],]
          Fitness[ord[1:elitism]] <- FitnessSorted[u[1:elitism]]
          object@population <- Pop
          object@fitness <- Fitness
        }

  }

  # in case of premature convergence remove NA from summary fitness evalutations
  object@summary <- na.exclude(object@summary)
  attr(object@summary, "na.action") <- NULL

  # get solution(s)
  object@fitnessValue <- max(object@fitness, na.rm = TRUE)
  valueAt <- which(object@fitness == object@fitnessValue)
  solution <- object@population[valueAt,,drop=FALSE]
  if(nrow(solution) > 1)
    { # find unique solutions to precision given by default tolerance
      solution <- unique(round(solution/eps)*eps, margin = 1)
    }
  colnames(solution) <- parNames(object)
  object@solution <- solution
  if(keepBest)
    object@bestSol <- object@bestSol[!sapply(object@bestSol, is.null)]

  # return an object of class 'ga'
  return(object)
}

setClassUnion("numericOrNA", members = c("numeric", "logical"))

setClass(Class = "ga",
         representation(min = "numericOrNA",
                        max = "numericOrNA",
                        nBits = "numericOrNA",
                        names = "character",
                        popSize = "numeric",
                        iter = "numeric",
                        run = "numeric",
                        maxiter = "numeric",
                        suggestions = "matrix",
                        population = "matrix",
                        elitism = "numeric",
                        pcrossover = "numeric",
                        pmutation = "numericOrNA",
                        fitness = "numericOrNA",
                        summary = "matrix",
                        bestSol = "list",
                        fitnessValue = "numeric",
                        solution = "matrix"
                      ),
         package = "gasub"
)

setGeneric(name = "parNames",
           def = function(object, ...) { standardGeneric("parNames") }
          )

setMethod("parNames", "ga",
function(object, ...)
{
  names <- object@names
  nvars <- ncol(object@population)
  if(length(names) == 0)
    { names <- paste("x", 1:nvars, sep = "") }
  return(names)
})

gaMonitor <- function(object, digits = getOption("digits"), ...)
{
  fitness <- na.exclude(object@fitness)
  cat(paste("Iter =", object@iter,
            " | Fitness =", format(max(fitness), digits = digits), "\n"))
}

gaSummary <- function(x, ...)
{
  # compute summary for each step
  x <- na.exclude(as.vector(x))
  q <- fivenum(x)
  c(max = q[5], mean = mean(x), q3 = q[4], median = q[3], q1 = q[2], min = q[1])
}

##
## Binary GA operators
##

gabin_Population <- function(object, ...)
{
# Generate a random population of nBits 0/1 values of size popSize
  population <- matrix(as.double(NA), nrow = object@popSize, ncol = object@nBits)
  for(j in 1:object@nBits)
     { population[,j] <- round(runif(object@popSize)) }
  return(population)
}

gabin_lrSelection <- function(object,
                            r = 2/(object@popSize*(object@popSize-1)),
                            q = 2/object@popSize, ...)
{
# Linear-rank selection
# Michalewicz (1996) Genetic Algorithms + Data Structures = Evolution Programs. p. 60
  rank <- (object@popSize+1) - rank(object@fitness, ties.method = "random")
  prob <- q - (rank-1)*r
  sel <- sample(1:object@popSize, size = object@popSize,
                prob = pmin(pmax(0, prob), 1, na.rm = TRUE),
                replace = TRUE)
  out <- list(population = object@population[sel,,drop=FALSE],
              fitness = object@fitness[sel])
  return(out)
}

gabin_nlrSelection <- function(object, q = 0.25, ...)
{
# Nonlinear-rank selection
# Michalewicz (1996) Genetic Algorithms + Data Structures = Evolution Programs. p. 60
  rank <- (object@popSize + 1) - rank(object@fitness, ties.method = "random")
  prob <- q*(1-q)^(rank-1)
  sel <- sample(1:object@popSize, size = object@popSize,
                prob = pmin(pmax(0, prob), 1, na.rm = TRUE),
                replace = TRUE)
  out <- list(population = object@population[sel,,drop=FALSE],
              fitness = object@fitness[sel])
  return(out)
}

gabin_rwSelection <- function(object, ...)
{
# Proportional (roulette wheel) selection
  prob <- abs(object@fitness)/sum(abs(object@fitness))
  sel <- sample(1:object@popSize, size = object@popSize,
                prob = pmin(pmax(0, prob), 1, na.rm = TRUE),
                replace = TRUE)
  out <- list(population = object@population[sel,,drop=FALSE],
              fitness = object@fitness[sel])
  return(out)
}

gabin_tourSelection <- function(object, k = 3, ...)
{
# (unbiased) Tournament selection
  sel <- rep(NA, object@popSize)
  for(i in 1:object@popSize)
     { s <- sample(1:object@popSize, size = k)
       sel[i] <- s[which.max(object@fitness[s])]
     }
  out <- list(population = object@population[sel,,drop=FALSE],
              fitness = object@fitness[sel])
  return(out)
}

gabin_spCrossover <- function(object, parents, ...)
{
# Single-point crossover
  fitness <- object@fitness[parents]
  parents <- object@population[parents,,drop = FALSE]
  n <- ncol(parents)
  children <- matrix(as.double(NA), nrow = 2, ncol = n)
  fitnessChildren <- rep(NA, 2)
  crossOverPoint <- sample(0:n, size = 1)
  if(crossOverPoint == 0)
    { children[1:2,] <- parents[2:1,]
      fitnessChildren[1:2] <- fitness[2:1] }
  else if(crossOverPoint == n)
         { children <- parents
           fitnessChildren <- fitness }
       else
         { children[1,] <- c(parents[1,1:crossOverPoint],
                           parents[2,(crossOverPoint+1):n])
           children[2,] <- c(parents[2,1:crossOverPoint],
                           parents[1,(crossOverPoint+1):n])
         }
  out <- list(children = children, fitness = fitnessChildren)
  return(out)
}


gabin_uCrossover <- function(object, parents, ...)
{
# Uniform crossover
  parents <- object@population[parents,,drop = FALSE]
  n <- ncol(parents)
  u <- runif(n)
  children <- parents
  children[1:2, u > 0.5] <- children[2:1, u > 0.5]
  out <- list(children = children, fitness = rep(NA,2))
  return(out)
}

gabin_raMutation <- function(object, parent, ...)
{
# Uniform random mutation
  mutate <- parent <- as.vector(object@population[parent,])
  n <- length(parent)
  j <- sample(1:n, size = 1)
  mutate[j] <- abs(mutate[j]-1)
  return(mutate)
}
