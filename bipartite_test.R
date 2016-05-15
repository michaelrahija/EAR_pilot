#test bipartite

head(links.as)
tail(nodes.as)

links.as$to %in% nodes.as$id 

test <- links.as$from %in% nodes.as$id
missing <- links.as$from[!test]
nodes[nodes$id %in% missing,]
