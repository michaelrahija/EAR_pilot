#create vector of countries
#takes vector containing elements. 
#will parse names of countries into a single vector 
#and return vector of country names


countriesVec <- function(vec = data.c$country){
  
  #clean names
  vec <- strsplit(vec,",")
  vec <-  unlist(vec)
  
  
  toreplace <- c(#" R?publique de la",
                 " Federal Democratic Republic of",
                 " (Republic of)",
                 "Republic",
                 " Republic of",
                 " The Republic of",
                 "United of",
                 " Democratic",
                 "( of)")
  
  for(i in 1:length(toreplace)){
    vec <- gsub(toreplace[i],"",vec)
  }
  
  vec <- gsub("^ ","",vec)
  
  vec <- countrycode(vec, origin = "country.name",
                     destination = "country.name")
  
vec  
}