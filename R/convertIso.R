#convert country names of data frame to iso3c and return data frame

convertIso <- function(df = NA){
  countries <- df[df$continent.type !=6,]
  
  countries$name <- countrycode(countries$name,
                                origin = "country.name",
                                destination = "iso3c")
  
  donors <- df[df$continent.type == 6,]
  
  df <- rbind(countries, donors)
  
    
df 
  
}