
library(dplyr)
library(xlsx)
library(tidyr)
library(countrycode)
library(XLConnect)


sys <- Sys.info()

if(sys[7] == "josh"){
  dir = "~/Documents/Github/EAR_pilot/"
} else if(sys[5] == "x86_64"){
  dir = "~/Dropbox/FAO_ESS_STUFF/EAR_pilot" #Mac
  data.dir = "~/Dropbox/FAO_ESS_STUFF/EAR_pilot_data/" 
} else if (sys[5] == "Michael"){
  dir = "C:/Users/Michael/Dropbox/FAO_ESS_STUFF/EAR_pilot"#HOME PC
  data.dir = "C:/Users/Michael/Dropbox/FAO_ESS_STUFF/EAR_pilot_data/"
} else if (sys[6]=="Rahija") {
  dir = "C:/Users/rahija/Dropbox/FAO_ESS_STUFF/EAR_pilot" #FAO PC
  data.dir = "C:/Users/rahija/Dropbox/FAO_ESS_STUFF/EAR_pilot_data/"
} else {
  stop("Implement location for current user!")
}

setwd(dir)

#Import data, and filter for operationally active, active pipeline, final consultation
active <- read.csv(file = paste0(data.dir,"operationally_active_06_05_16.csv"),
                   stringsAsFactors = FALSE)
active <- filter(active, Project.Status == "Operationally Active")

pipe <- read.csv(file = paste0(data.dir,"pipeline_06_05_16.csv"),
                 stringsAsFactors = FALSE)

pipe <- filter(pipe, Project.Status == "Active Pipeline" | Project.Status == "Final Consultation")


#Select only relevant columns
active <- select(active,
                 Actual.NTE,
                 Country.Name,
                 Geographical.Coverage,#for filtering later
                 Project.Staff...Funding.Officer,
                 Project.Staff...LTO.Officer,
                 Project.Staff...LTU.Officer,
                 Project.Status,
                 Project.Symbol,
                 Project.Title, #to apply addTeam function
                 Total.Budget..FPMIS.) 

pipe <-select(pipe,
              Actual.NTE,
              Country.Name,
              Geographical.Coverage,#for filtering later
              Project.Staff...Funding.Officer,
              Project.Staff...LTO.Officer,
              Project.Staff...LTU.Officer,
              Project.Status,
              Project.Symbol,
              Project.Title, #to apply addTeam function
              Total.Budget..FPMIS.) 

master <- rbind(active, pipe) #some column names not matching....

#create staff column, to use addTeam function

master$staff <- paste0(master$Project.Staff...Funding.Officer,
                       master$Project.Staff...LTO.Officer,
                       master$Project.Staff...LTU.Officer,
                       sep = ";")

master$staff <- gsub(";$", "",master$staff)

#clean up column names and project titles
cols <- colnames(master) 
cols <- gsub("\\.","",cols)
colnames(master) <- cols

#clean up project names - still some to do!
master$ProjectTitle <- gsub("\\\x82","e",master$ProjectTitle)
master$ProjectTitle <- gsub("\\?","'",master$ProjectTitle)
master$ProjectTitle <- gsub("\xfc\xbe\x8c\xa6\x84\xbc","o", master$ProjectTitle)
master$ProjectTitle <- gsub("\xfc\xbe\x8c\xb6\x84\xbc","u",master$ProjectTitle)

#clean country
master$CountryName <- gsub("\\\x93","o",master$CountryName)
master$CountryName <- gsub("\\\x82","e",master$CountryName)


#-Add Team column
#Add team - DOUBLE CHECK TEAMS!
source("R/addTeam.R")
master  <- addTeam(master)


#-Filter for country projects, clean name
x <- filter(master, GeographicalCoverage == "Country")
x$CountryName <- countrycode(x$CountryName,
                                  origin = "country.name",
                                  destination = "country.name")
x <- filter(x, !is.na(CountryName))

x <- select(x, CountryName, team, ActualNTE, ProjectSymbol, TotalBudgetFPMIS)

#-id duplicated rows, concatenate w/ letter, put back in df
x$dup <- duplicated(paste0(x$CountryName, x$team))
xx <- filter(x, dup == FALSE)

y <- filter(x, dup == TRUE)
y$CountryName <- paste0(y$CountryName,"_",sample(letters,nrow(y)))

x <- rbind(xx,y)

if(sum(duplicated(paste0(x$CountryName,x$team)))) stop("duplicated country names")

#-create df for each team
teams <- unique(x$team)
team.list <- list()

for(i in 1:length(teams)){
  temp <- select(x,-dup)
  team.list[[i]] <- filter(temp, team == teams[i])
  
  colnames(team.list[[i]]) <- c("Country",
                                "team",
                                paste0(teams[i],"_","NTE"),
                                paste0(teams[i],"_","Symbol"),
                                paste0(teams[i],"_","Budget"))#,
                                #paste0(teams[i],"_","dup"))
  team.list[[i]] <- select(team.list[[i]], 
                           -team)
}

#- merge dfs from list into 1 data frame
master <- team.list[[1]]

for(i in 2:length(team.list)){
  master <- merge(master, team.list[[i]],
                  by = "Country",
                  all = TRUE)
}

#-Clean up Project Symbols and create 1 column 
symbol.cols <- grep("symbol", 
                    colnames(master),
                    ignore.case = TRUE)

test<- master[, symbol.cols]
