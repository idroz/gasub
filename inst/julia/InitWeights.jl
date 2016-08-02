function InitWeights(json)
  weight = json["weights"]

  convert(Array{Float64,1}, weight)
end
