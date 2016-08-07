function Crossover(x, y)
  crossover_length = reshape(sample(1:(round(Int64, length(x)/2)), 1), 1)[1]
  crossover_position = reshape(sample(1:(length(x) - crossover_length), 1))[1]

  x[crossover_position:(crossover_position + crossover_length)] = y[crossover_position:(crossover_position + crossover_length)]
  x
end
