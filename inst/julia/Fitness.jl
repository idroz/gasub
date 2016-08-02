function Fitness(graph, weights, permutation)
  index = find(permutation .== 1)
  subg = induced_subgraph(graph, index)
  subweights = weights[index]

  neighbour_weights = Array{Float64}(nv(subg), 1)

  for i = 1:nv(subg)
    neighbour_weights[i] = sum(subweights[neighbors(subg, i)])
  end

  sum(subweights .* neighbour_weights) / sqrt(nv(subg))

end
