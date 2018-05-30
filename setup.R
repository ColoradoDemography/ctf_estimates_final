# Revision to CTF program Adam Bickford May-June 2018
# There are many changes, including updates to tidyverse,
# addition of code to read data from the postgres database,
# and update of the data processing steps to read the new
# data tables.

# Use this program to read in the data sets.
# Save the processing for a function in server.R

rm(list=ls())
library(tidyverse)
library(knitr)
library(kableExtra)
library(RPostgreSQL)


#Read data from the Postgres database

#Prepping SQL calls
RCSSQL <- "SELECT * FROM data.ctf_rcs_names;"
POPESTSQL <- "SELECT * FROM data.ctf_pop_est;"
CONSTRSQL <- "SELECT * FROM data.ctf_construction;"

# Call to Postrgres  
pw <- {
  "demography"
}

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = 'dola',
                 host = "104.197.26.248", port = 5433,
                 user = "codemog", password = pw)
rm(pw) # removes the password

# Read data files
areas <- dbGetQuery(con,RCSSQL)
popdata <- dbGetQuery(con,POPESTSQL)
housedata <- dbGetQuery(con, CONSTRSQL) 


#closing the connections
dbDisconnect(con)
dbUnloadDriver(drv)
rm(con)
rm(drv)

#Modify Place Names file
areas$locid <- ifelse(grepl("Unincorporated",areas$place),1,0)
areas <- areas %>% arrange(locid, place) %>%
  mutate(county = paste0(county, " County"),
         id=paste0(countyfips,placefips))
areas <- areas[,c(1:5,7)]

# Population Estimates data file

popdata <- popdata %>% mutate(id=paste0(countyfips,placefips)) 
sdopopdata <- popdata[,c(18,1:7,17,8:15)]

# Building permit data files

housedata <- housedata %>% mutate(id=paste0(countyfips,placefips))
sdohousedata <- housedata[,c(11,1:4,6:9)]



#Extract census variables
# removing county totals
popdata2 <- popdata[which(popdata$placefips != "00000"),]
cpop <- popdata2[,c(1,2,18,7,17,16)]

# Census  Population  reconstructing totals
cpop1 <- cpop[which(cpop$countyfips != "999"),] 
cpop2 <- cpop[which(cpop$countyfips == "999"),] 
uniqmulti <- unique(cpop2$placefips)

cpopmulti <- subset(cpop, placefips %in% uniqmulti)  # this is the list of contains the multi county places for summarization
cpopmulti <- cpopmulti[which(cpopmulti$countyfips != "999"),]
sumpop <- cpopmulti %>% group_by(placefips,vartype) %>% 
          summarize(censuspop = sum(censuspop)) 
sumpop$countyfips <- "999"
sumpop$id <- ""
sumpop$year <- as.numeric(0)
sumpop <- sumpop[,c(4,1,5,6,2,3)]

# cpopdata contiains the census population
cpopdata <- bind_rows(cpop1,sumpop)


#SDO Housing data
sdohousmulti <- subset(sdohousedata,placefips %in% uniqmulti)
sdohousmulti <- sdohousmulti[which(sdohousmulti$countyfips != "999"),]
sumhouse <- sdohousmulti %>% group_by(placefips,year) %>% 
  summarize(localbp = sum(localbp),
            localdemo = sum(localdemo),
            localco = sum(localco),
            localmhc = sum(localmhc)
            ) 
sumhouse$countyfips <- "999"
sumhouse$id <- ""
sumhouse$areaname <- ""

sumhouse <- sumhouse[,c(8,7,1,9,2:6)]
sdohousedata <- bind_rows(sdohousedata,sumhouse)

# census Housing data

# removing county totals
housedata2 <- housedata[which(housedata$placefips != "00000"),]
chouse <- housedata2[,c(1,2,11,4,5,10)]

# extracting multi-county places and summarizing

chousemulti <- subset(chouse, placefips %in% uniqmulti)  # this is the list of contains the multi county places for summarization

sumhouse <- chousemulti %>% group_by(placefips,year) %>% 
  summarize(censusbp = sum(censusbp),
            censushu = sum(censushu)) 
sumhouse$countyfips <- "999"
sumhouse$id <- ""
sumhouse <- sumhouse[,c(5,1,6,2:4)]

chousedata <- bind_rows(chouse,sumhouse)

# Now have 4 data sets:
# 1) sdopopdata: SDO population estimates, long key vars: countyfips, placefips vartype
# 2) cpopdata: Census population estimates, long key vars: countyfips, placefips vartype
# 3) sdohousedata:  SDO building permit/housing estimates, long key vars: countyfips, placefips year
# 4) chousedata:  Census building permit/housing estimates, long key vars: countyfips, placefips year

# Creating wide SDo Population data set

sdopop <- sdopopdata[,c(1,2,3,9,15,10,11,13,14,12,16,17)]

# Formatting Values
sdopop$tp <-  formatC(sdopop$tp, format="d", big.mark=",")
sdopop$groupquarters <-  formatC(sdopop$groupquarters, format="d", big.mark=",")
sdopop$hp <-  formatC(sdopop$hp, format="d", big.mark=",")
sdopop$pph <-  format(sdopop$pph, nsmall=2)
sdopop$thu <-  formatC(sdopop$thu, format="d", big.mark=",")
sdopop$ohu <-  formatC(sdopop$ohu, format="d", big.mark=",")
sdopop$vhu <-  formatC(sdopop$vhu, format="d", big.mark=",")
sdopop$vr <-  format(sdopop$vr, nsmall=2)

sdopop2 <- sdopop %>% gather(popname,val, -id, -countyfips, -placefips, -vartype) %>% filter(placefips != "00000") 
sdopop3 <- unique(sdopop2) %>%  spread(vartype, val)

sdopop3 <- sdopop3[,c(1:4,6,7,5,8:14)]

#setting variable values
sdopop3$popname <- ifelse(sdopop3$popname == "tp","Total Population",
                   ifelse(sdopop3$popname == "groupquarters","Group Quarters Population",
                   ifelse(sdopop3$popname == "hp","Household Population",
                   ifelse(sdopop3$popname == "pph","Persons Per Household",
                   ifelse(sdopop3$popname == "thu","Total Housing Units",
                   ifelse(sdopop3$popname == "ohu","Occupied Housing Units",
                   ifelse(sdopop3$popname == "vhu","Vacant Housing Units","Vacancy Rate")))))))
sdopop3$popname <- factor(sdopop3$popname, levels = c("Total Population", "Group Quarters Population",
                                                      "Household Population", "Persons Per Household",
                                                      "Total Housing Units", "Occupied Housing Units",
                                                      "Vacant Housing Units","Vacancy Rate"))
# cpopdata

cpop1 <- cpopdata[,c(3,1,2,5,6)]
cpop1$censuspop <- formatC(cpop1$censuspop, format="d", big.mark=",")

cpop2 <- cpop1 %>% gather(popname,val, -id, -countyfips, -placefips, -vartype) 
cpop3 <- unique(cpop2) %>%  spread(vartype, val)
cpop3 <- cpop3[,c(1:4,6,7,5,8:14)]
cpop3$id <- paste0(cpop3$countyfips,cpop3$placefips)
cpop3$popname <- "Total Population"

#SDO Housing
sdobp <- sdohousedata[,c(1,2,3,5:9)]
sdobp$id <- paste0(sdobp$countyfips,sdobp$placefips)
sdobp$localbp <- formatC(sdobp$localbp, format="d", big.mark=",")
sdobp$localdemo <- formatC(sdobp$localdemo, format="d", big.mark=",")
sdobp$localco <- formatC(sdobp$localco, format="d", big.mark=",")
sdobp$localmhc <- formatC(sdobp$localmhc, format="d", big.mark=",")

sdobp2 <- sdobp %>% gather(popname,val, -id, -countyfips, -placefips, -year) %>% filter(placefips != "00000") 
sdobp3 <- unique(sdobp2) %>%  spread(year, val)


sdobp3$popname <- ifelse(sdobp3$popname == "localbp","Local Building Permits",
                  ifelse(sdobp3$popname == "localdemo","Local Demolition Permits",
                  ifelse(sdobp3$popname == "localco","Local Certificates of Occupancy","Local Mobile Home Change")))

sdobp3$popname <- factor(sdobp3$popname, levels = c("Local Building Permits","Local Certificates of Occupancy",
                         "Local Demolition Permits", "Local Mobile Home Change"))


# Census Housing
 cbp <- chousedata[,c(3,1,2,4:6)]
 cbp$id <- paste0(cbp$countyfips,cbp$placefips)
 cbp$censushu <- formatC(cbp$censushu, format="d", big.mark=",")
 cbp$censusbp <- formatC(cbp$censusbp, format="d", big.mark=",")
 
 cbp2 <- cbp %>% gather(popname,val, -id, -countyfips, -placefips, -year) 
 cbp3 <- unique(cbp2) %>%  spread(year, val)
 
 cbp3$popname <- ifelse(cbp3$popname == "censushu","Census Housing Units","Census Building Permits")
 cbp3$popname <- factor(cbp3$popname, levels=c("Census Housing Units","Census Building Permits"))
 
 
