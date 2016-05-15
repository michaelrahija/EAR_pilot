#create maps

#create maps and figures for analysis

#-- import data and packages
library(DT)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(maps)
library(countrycode)
library(scales)
library(igraph)
library(gisfao)
library(sp)

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

# 
# data <- read.csv(paste0(data.dir,"agstats_final.csv"), 
#                  stringsAsFactors = FALSE)
data <- read.csv(paste0(data.dir,"agstats_final_for narrative.csv"), 
                 stringsAsFactors = FALSE)




source("R/cleanFigures.R")
data <- cleanFigures(data) 

##THIS INTRODUCES NAs IN BUDGET B/C LESLIE INCLUDED SOME TEXT


#--------CREATE DF FOR EXPENDITURES BY COUNTRY AND MERGE CENTROIDS

#Get expenditure data 
test.c <- colnames(data) %in% NA
data <- data[!test.c]


#take only baby project and NA
data.c <- filter(data, is.na(baby) | baby == 1)

#remove VoH, GS, and CARDS
data.c <- filter(data.c, GsVohCard == 0)

#remove 'Unallocated' countries
data.c <- filter(data.c, country != "Unallocated")

#data.d <- select(data.c, country,budget)
data.d <- select(data.c, country, budget, donor)
data.d <- filter(data.d, !is.na(budget))

x <- strsplit(data.d$country,",")

data.d$denom <- sapply(x,length)

options(scipen = 999)

data.d$totl <- data.d$budget/data.d$denom

#HACK TO REMOVE DUPS
test <- duplicated(data.d$totl)
data.d$totl[test] <- data.d$totl[test] + .1

names(x) <- data.d$totl

x1 = lapply(names(x), function(name){
  data.frame(amount = name,
             country = x[[name]])
})

master <- do.call("rbind", x1)
master$country <- as.character(master$country)
master$amount <- as.numeric(as.character(master$amount))
master$country <- countrycode(master$country, origin = "country.name",
                                destination = "country.name", warn = TRUE)

master <- master %>% 
            group_by(country) %>%
            summarize(amount = sum(amount))


#Get data frame w/ polygons and centroids
centroid <- as.data.frame(fao_world_centroids)
centroid <- select(centroid,ADM0_NAME,x,y)
colnames(centroid) <- c("country","clong","clat")
centroid$country <- as.character(centroid$country)

temp <- data.frame(country = c("Cabo Verde","Comoros", "Kiribati", "Tonga"),
                   clong = c(-23.627155,43.335468,-157.409388,-175.161552),
                   clat = c(15.088946,-11.664959,1.881687,-21.172421))
centroid <- rbind(temp,centroid)

centroid$country <- countrycode(centroid$country, origin = "country.name",
                                destination = "country.name", warn = TRUE)

master <- merge(master,centroid, by = "country", all = TRUE)

master <- filter(master, !is.na(amount))
master <- filter(master, !(country == "India" & clat > 23))

if(sum(duplicated(master$country)) > 0) print("duplicated country!")


fao_world2 = fortify(fao_world)
countryName = fao_world@data
countryName$id = rownames(countryName)

fao_world2 = merge(fao_world2, countryName, by = "id")

#filter data
master.t <- filter(master,!is.na(amount))
fao_world2 <- filter(fao_world2, ADM0_NAME != "Antarctica")
fao_world2 <- arrange(fao_world2,order)

master.t$country  <- gsub("^ +","", master.t$country)

##---CREATE DF FOR DONORS BY COUNTRY AND MERGE
donors <- select(data.d,country,donor)
x <- strsplit(donors$country,",")

names <- donors$donor
test <- data.frame(names = as.character(names), rep = sapply(x,length))
y <- c()

#create vector of donors
for(i in 1:nrow(test)){
  temp <- rep(as.character(test[i,1]), times = test[i,2])
  y <- append(y,temp)
}

#bind donors w/ unlisted countries
master.c <- data.frame(donors = y, country = unlist(x))
master.c$country <- as.character(master.c$country)
master.c$country <- countrycode(master.c$country, origin = "country.name",
                              destination = "country.name", warn = TRUE)

master.c <- master.c %>%
              group_by(country) %>%
              summarize(donors = n())


master <- merge(master, master.c, all = TRUE)



## PLOT


#number of groups 
n = 5
groups = 16000000/n
breaks = groups * 1:n

#format donors variables
names(master)[names(master) == "donors"] <- "Donors"
ggplot() +
  geom_polygon(data=fao_world2, aes(x=long, y=lat, group = group),colour="darkgrey", fill="white" ) +
  theme_classic() +
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  geom_point(data=master, aes(x=clong, y=clat, size = amount, fill = Donors), shape = 21, color = "lightgrey") +
  scale_fill_gradient(low = "yellow", high = "olivedrab") +
  scale_size(#max_size = 10,
                  range = c(3,11),
                  breaks = breaks,
                  name = "Total Funding \n(million USD)",
                  labels = c("1", "5",
                             "10","15")) + 
  geom_text(size=4) 


