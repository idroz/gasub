function InitParameters(json)
  popsize = json["popsize"]
  pmutation = json["pmutation"]
  eletism = json["eletism"]
  maxiter = json["maxiter"]
  run = json["run"]

  params = Array{Any}(1,5)

  params[1] = popsize
  params[2] = pmutation
  params[3] = eletism
  params[4] = maxiter
  params[5] = run

  return params
end
