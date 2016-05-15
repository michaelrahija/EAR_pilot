#class FAO relationships
#will take a nodes and links dfs and append column
#to nodes classifying countries as 1 = FAO only
# 2 = FAO + others, 3 = others, 4  = donor

faoClass <- function(links = links, nodes = nodes){
  
  #first id FAO's id
  fao.id <- nodes$id[nodes$name == "FAO"]
  donor.id <- nodes$id[nodes$continent.type == 6]
  
  #create master dataset for later merger into nodes
  master <- data.frame(to = nodes$id, faoClass = NA)
  
  #create vector of ids receiving FAO
  fao <- links$to[links$from == fao.id]
  
  #create vector of ids not receiving FAO
  other <- links$to[links$from != fao.id]
  
  #test which ids are both list to id fao and other, and which are fao only
  fao.with.test <- fao %in% other
  
  fao.with <- fao[fao.with.test]
  
  fao.only <- fao[!fao.with.test]
  
  master$faoClass[master$to %in% fao.with] <- 2
  master$faoClass[master$to %in% fao.only] <- 1
  master$faoClass[is.na(master$faoClass)] <- 3
  master$faoClass[master$to %in% donor.id] <- 4

  master <- merge(nodes, master, by.x = "id", by.y = "to", all = TRUE)

master  
  
  }