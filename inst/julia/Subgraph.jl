using Distributions
using JSON
import LightGraphs; @everywhere using LightGraphs

include("ReadOptions.jl")
include("InitGraph.jl")
include("InitWeights.jl")
include("InitParameters.jl")


include("InitPopulation.jl")
include("Crossover.jl")
include("Fitness.jl")
include("GA.jl")

filename = ARGS[1]
popoutname = ARGS[2]
fitnessoutname = ARGS[3]

json = ReadOptions(filename)
graph = InitGraph(json)
weights = InitWeights(json)
params = InitParameters(json)

res = GA(graph, weights, params[1], params[4], params[5], params[3], params[2], params[6])

score = res["populationfitness"]
bestpop = find(score .== maximum(score))[1]

population = res["population"]
best = population[bestpop, :]
fitness = res["generationfitness"]

writedlm(popoutname, best, ", ")
writedlm(fitnessoutname, fitness, ", ")
