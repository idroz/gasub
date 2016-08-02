function Crossover(x, y)
  area = find(rand(Uniform(), 1, length(x)) .> 0.5)
  x[area] = y[area]
  x
end
