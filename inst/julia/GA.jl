function GA(graph, weights, popsize, maxiter, eletism, pmutation)

  nbits = nv(graph)
  population = InitPopulation(popsize, nbits)
  maxscore = zeros(maxiter, 1)
  score = Array{Float64}(popsize)


  for n = 1:maxiter

    for i = 1:popsize
      score[i] = Fitness(graph, weights, population[i,:])
    end

    ordered = sortperm(score, rev = true)
    elites = population[ordered[1:eletism], :]

    f = maxscore[n] = maximum(score)


    # Selection
    prob =  abs(score) ./ sum(abs(score))
    sel = wsample(1:popsize, min(max(0, prob), 1), popsize)
    population = population[sel,:]

    # Crossover
    nmating = Int(floor(popsize/2))
    mating = reshape(sample(1:popsize, popsize, replace = false), nmating, 2)
    crossover = Array{Int64}(nmating, nbits)

    for i=1:nmating
      crossover[i,:] = Crossover(population[mating[i, 1], :], population[mating[i, 2], :])
    end
    population[mating[:, 1], :] = crossover

    # Mutation
    idx = find(rand(Uniform(), popsize) .< pmutation)
    mutation = sample(1:nbits, length(idx))
    population[idx, mutation] = abs(population[idx, mutation]-1)

    # Eletism
    population[ordered[1:eletism],:] = elites

    println("Iteration: $n | Fitness: $f")
  end


  Dict("population" => population, "fitness" => score)
end
