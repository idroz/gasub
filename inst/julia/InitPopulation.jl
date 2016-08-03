function InitPopulation(popsize::Int, nbits::Int)
  println("Initialised population with $popsize chromosomes and $nbits bits.")
  rand(Binomial(), popsize, nbits)
end
