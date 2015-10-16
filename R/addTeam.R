#add team column

addTeam <- function(data.o){
  
  data.o$team <- rep(NA, n= nrow(data.o))
  
  
  #####################################
  ## START BY USING PROJECT TITLE    ##
  #####################################
  
  #fill in census(team H)
  census.index <- grepl("census",data.o$ProjectTitle, 
                        ignore.case = TRUE)
  data.o$team[census.index] <- "Team H"
  
  censo.index <- grepl("censo",data.o$ProjectTitle, 
                       ignore.case = TRUE)
  data.o$team[censo.index] <- "Team H"
  
  recen.index <- grepl("recense", data.o$ProjectTitle,
                       ignore.case = TRUE)
  data.o$team[recen.index] <- "Team H"
  
  #fill in team G
  gs.index <- grepl("global strategy", data.o$ProjectTitle,
                    ignore.case = TRUE)
  data.o$team[gs.index] <- "Team G"
  
  spar.index <- grepl("strategic plan", data.o$ProjectTitle,
                      ignore.case = TRUE)
  data.o$team[spar.index] <- "Team G"
  
  amis.index <- grepl("agriculture market information", data.o$ProjectTitle,
                      ignore.case = TRUE)
  
  data.o$team[amis.index] <- "Team G"
  
  amis2.index <- grepl("agricultural market information", data.o$ProjectTitle,
                      ignore.case = TRUE)
  
  data.o$team[amis2.index] <- "Team G"
  
  #fill in team d - food security
  fs.index <- grepl("food security", data.o$ProjectTitle,
                    ignore.case = TRUE)
  data.o$team[fs.index] <- "Team D"
  
  fsi.index <- grepl("food insecurity", data.o$ProjectTitle,
                     ignore.case = TRUE)
  data.o$team[fsi.index] <- "Team D"
  
  sa.index <- grepl("seguridad", data.o$ProjectTitle,
                       ignore.case = TRUE)
  data.o$team[sa.index] <- "Team D"
  
  sa.index <- grepl("sex disaggregated", data.o$ProjectTitle,
                    ignore.case = TRUE)
  data.o$team[sa.index] <- "Team D"
  
  sa.index <- grepl("sex-disaggregated", data.o$ProjectTitle,
                    ignore.case = TRUE)
  data.o$team[sa.index] <- "Team D"
  
  fr.index <- grepl("s.*curit.* alimentaire", data.o$ProjectTitle,
                    ignore.case = TRUE)
  data.o$team[fr.index] <- "Team D"
  
  #fill in team k - voice of the hungry
  voh.index <- grepl("voices", data.o$ProjectTitle,
                     ignore.case = TRUE)
  data.o$team[voh.index] <- "Team K"
  
  #fill in team i - countrystat
  cs.index <- grepl("countrystat",data.o$ProjectTitle,
                    ignore.case = TRUE)

  data.o$team[cs.index] <- "Team I"  
  
  
  
  #fill in team b - food balance
  fbs.index <- grepl("food balance",data.o$ProjectTitle,
                    ignore.case = TRUE)
  
  data.o$team[fbs.index] <- "Team B" 
  
  
  completed <- filter(data.o, !is.na(team))
  
  #######################################
  ## FILL IN PROJECTS WHERE TEAM CANNOT #
  ## BE DETERMINED BY PROJECT TITLE BY  #
  ## USING STAFF COLUMN                 #
  #######################################
  
  #to be filled in using staff
  uncomplete <- filter(data.o, is.na(team)) 
  
  #use Boero for team RLC
  rlc.index <- grepl("boero", uncomplete$staff,
                     ignore.case = TRUE)
  
  uncomplete$team[rlc.index] <- "RLC" 
  

  #use Ouedraogo for RAF
  raf.index <- grepl("ouedraogo", uncomplete$staff,
                     ignore.case = TRUE)
  
  uncomplete$team[raf.index] <- "RAF" 
  
  #use srivastava for RAP
  rap.index <- grepl("srivastava", uncomplete$staff,
                     ignore.case = TRUE)
  
  uncomplete$team[rap.index] <- "RAP"
  
  #use barre for RNE
  rne.index <- grepl("barre", uncomplete$staff,
                     ignore.case = TRUE)
  
  uncomplete$team[rne.index] <- "RNE"
  
  #use chin for FAOSLC
  slc.index <- grepl("chin", uncomplete$staff,
                     ignore.case = TRUE)
  
  uncomplete$team[slc.index] <- "SLC"
  
  #use wall for FAOSAP
  sap.index <- grepl("wall, g.", uncomplete$staff,
                     ignore.case = TRUE)
  
  uncomplete$team[sap.index] <- "SAP"
  
  #use Grita for team I
  grita.index <- grepl("grita", uncomplete$staff,
                     ignore.case = TRUE)
  
  uncomplete$team[grita.index] <- "Team I"
  
  #use Grita for team I
  josef.index <- grepl("schmidhuber", uncomplete$staff,
                       ignore.case = TRUE)
  
  uncomplete$team[josef.index] <- "Team J"
  
  
  ###############################
  ## replace NAs with not found##
  ###############################
  na.index <- is.na(uncomplete$team)
  
  uncomplete$team[na.index] <- "NA"

rbind(completed, uncomplete)  
}