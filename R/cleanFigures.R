#clean data for figures


cleanFigures <- function(df = data) {
  
  colnames(df) <- gsub("\\.","",colnames(data))
  
  if (colnames(df)[1] == "X") df <- data[,-1]
  
  colnames(df) <- c("source",
                      "donor",
                      "implementer",
                      "country",
                      "supportType",
                      "Region",
                      "start",
                      "finish",
                      "budget",
                      "title",
                      "projectType",
                      "assCountry",
                      "projectSymbol",
                      "baby",
                      "GsVohCard",
                      "comments")
  
  
  df$budget <- as.numeric(gsub(",","",df$budget))
  
  df$country[df$country == "C\xf4te d'Ivoire"] <- "Cote d'Ivoire"
  df$country[df$country == "C\x99te d'Ivoire"] <- "Cote d'Ivoire"
  df$country[df$country == "Cameroon"] <- "Cameroun"

  #drop blank rows
  df <- filter(df, !is.na(supportType))
  
  #combine 
  df$donor[df$donor == "U.S. State Department"] <- "USA"
  df$donor[df$donor == "USAID"] <- "USA"
  df$donor[df$donor == "JICA"] <- "Japan"

  #trim white space at end
  trim.trailing <- function (x) sub("\\s+$", "", x)
  df$projectType <- trim.trailing(df$projectType)
  
  trim.trailing.comma <- function (x) sub("\\,+$", "", x)
  df$country <- trim.trailing.comma(df$country)
  
  trim.trailing.comma.space <- function (x) sub("\\,+ $", "", x)
  df$country <- trim.trailing.comma.space(df$country)
  
df 
  
  
}