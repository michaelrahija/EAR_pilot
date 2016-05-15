#clean project titles, change special characters. 
#Takes a vector of strings, and searches and replaces specific characters

replaceAccents <- function(vector = NA){
  
  #first replace Ž
  vector <- gsub("Ž","é", vector)

vector  
  
}