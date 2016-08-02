function InitPopulation(popsize::Int, nbits::Int)
  println("Initialised poopulation with $popsize chromosomes and $nbits bits.")
  rand(Binomial(), popsize, nbits)
end
