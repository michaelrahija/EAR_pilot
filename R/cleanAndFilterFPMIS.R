#This script cleans up FPMIS data

cleanAndFilterFPMIS <- function(data, ActiveOnly = FALSE, fixDate = FALSE){
  
  #clean up column names
  cols <- colnames(data) 
  cols <- gsub("\\.","",cols)
  colnames(data) <- cols
  
  #convert date columns from characters
#   data$ActualEOD <- as.POSIXct(data$ActualEOD,"mm/dd/YY")
#   data$ActualNTE <- as.POSIXct(data$ActualNTE,"YYYY-MM-DD")

  if(fixDate == TRUE){
  data$ActualNTE <-as.Date(data$ActualNTE, format = "%m/%d/%Y")
  data$ActualEOD <-as.Date(data$ActualEOD, format = "%d/%b/%Y")
  }

  #redefine data frame
  data.o <- data

  #filter for 'operationally active'
  if(ActiveOnly == TRUE){
  data.o <- filter(data, ProjectStatus == "Operationally Active")
  }


  data.o <- unique(data.o)
  data.o$ProjectTitle <- gsub("^ *","",data.o$ProjectTitle)
#   
#   #edit multilateral donors for when there is one donor shown as 100%
#   test1 <- grepl("100%",data.o$Donor) 
#   data.o$Donor[test1] <- 
#   
  data.o
}