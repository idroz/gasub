function InitGraph(json)
  num_nodes = length(json["vertexlist"])
  edges = json["edgelist"]
  graph = Graph(num_nodes)


  for i = 1:length(edges)
    add_edge!(graph, edges[i][1], edges[i][2])
  end

  graph
end
