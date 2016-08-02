using Distributions
using JSON
using LightGraphs

include("ReadOptions.jl")
include("InitGraph.jl")
include("InitWeights.jl")
include("InitParameters.jl")

include("InitPopulation.jl")
include("Crossover.jl")
include("Fitness.jl")
include("GA.jl")

filename = ARGS[1]
outname = ARGS[2]

json = ReadOptions(filename)
graph = InitGraph(json)
weights = InitWeights(json)
params = InitParameters(json)

res = GA(graph, weights, params[1], params[4], params[3], params[2])

score = res["fitness"]
bestpop = find(score .== maximum(score))[1]

population = res["population"]
best = population[bestpop, :]

writedlm(outname, best, ", ")
