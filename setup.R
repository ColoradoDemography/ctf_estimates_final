# Revision to CTF program Adam Bickford June 2022
# There are many changes, including updates to tidyverse,
# addition of code to read data from the postgres database,
# and update of the data processing steps to read the new
# data tables.

# Use this program to read in the data sets.
# Save the processing for a function in server.R

rm(list=ls())
#setwd("J:/Estimates/CTF Estimates R programs/CTF Website Programs")
library(tidyverse)
library(readr)
library(stringr)
library(knitr)
library(kableExtra)
library(RPostgreSQL)


# Additions for Database pool  6/5/2019
library('pool') 
library('DBI')
library('config')

# Set up database pool 6/5/2019

config <- get("database")
DOLAPool <-  dbPool(
  drv <- dbDriver(config$Driver),
  dbname = config$Database,
  host = config$Server,
  port = config$Port,
  user = config$UID,
  password = config$PWD
)


#Read data from the Postgres database

#Prepping SQL calls
# file documentation in  J:\Estimates\CTF Estimates R programs\CTF File Documentation V2018.docx
RCSSQL <- "SELECT * FROM data.ctf_rcs_names;"  # List of Location Names [place name] in [County]
POPESTSQL <- "SELECT * FROM data.ctf_pop_est ORDER BY countyfips, vartype;"   # the Population.csv file...
CONSTRSQL <- "SELECT * FROM data.ctf_construction ORDER BY countyfips, year;"  #Building Permit file..."


# Read data files  Production
 areas <- dbGetQuery(DOLAPool,RCSSQL)  %>% arrange(place) %>% mutate(id=paste0(countyfips,placefips))
 popdata <- dbGetQuery(DOLAPool,POPESTSQL) %>% mutate(id=paste0(countyfips,placefips))
 housedata <- dbGetQuery(DOLAPool, CONSTRSQL) %>% mutate(id=paste0(countyfips,placefips))

 

#Read data Files Testing
# areas <- read_csv("J:/Estimates/CTF Estimates R programs/ctf_rcs_names.csv") %>%
#      mutate(countyfips = str_pad(countyfips,3,pad="0"),
#             placefips = str_pad(placefips,5,pad="0"))

# popdata <- read_csv("J:/Estimates/CTF Estimates R programs/ctf_pop_est.csv") %>%
#  mutate(countyfips = str_pad(countyfips,3,pad="0"),
#         placefips = str_pad(placefips,5,pad="0"))

# housedata <-  read_csv("J:/Estimates/CTF Estimates R programs/ctf_construction.csv") %>%
#  mutate(countyfips = str_pad(countyfips,3,pad="0"),
#         placefips = str_pad(placefips,5,pad="0"))


#closing the connections
poolClose(DOLAPool)

#Modify Place Names file


areas <- areas %>% 
  mutate(locid = ifelse(grepl("Unincorporated",areas$place),1,0),
         county = paste0(county, " County"),
         id=paste0(countyfips,placefips)) %>%
         arrange(locid, place) %>%
      select(name, place, county, countyfips, placefips, id)



# Population Estimates data file ctf_pop_est
#
# sdopopdata <- popdata %>% 
#        select(id, countyfips, placefips, multicountyplaceflag, munitotalflag, 
#               adjustmentarea, areaname, year, vartype, groupquarters, 
#               hp, thu, tp, hpthuratio)
# 2023 Vintage

 sdopopdata <- popdata %>% 
        select(id, countyfips, placefips, multicountyplaceflag, munitotalflag, 
               adjustmentarea, areaname, year, vartype, groupquarters, 
               hp, thu, tp, ohu, pph, vhu, vr)

# Building permit data files ctf_construction

sdohousedata <- housedata %>% mutate(id = paste0(countyfips,placefips)) %>%
  select(id, countyfips, placefips, areaname, year, localbp, localdemo, localco)




#Extract census variables ctf_pop_est
# removing county totals -- need to calculate multi...

popdata2 <- popdata %>% filter(placefips != "00000")
cpop <- popdata2 %>% select(countyfips, placefips, id, year, vartype, censuspop)

# Census  Population  reconstructing totals

cpop1 <- cpop %>% filter(countyfips != "999") %>% mutate(year = as.character(year))
cpop2 <- cpop %>% filter(countyfips != "999") 
uniqmulti <- unique(cpop2$placefips)

cpopmulti <- cpop %>% filter(placefips %in% uniqmulti) %>%  # this is the list of contains the multi county places for summarization
             filter(countyfips != "999")


sumpop <- cpopmulti %>% group_by(placefips,vartype) %>% 
          summarize(censuspop = sum(censuspop)) %>%
          mutate(countyfips = "999",
                 id = "",
                 year = "0") %>%
         select(countyfips, placefips, id, year, vartype, censuspop)

# cpopdata contiains the census population
cpopdata <- bind_rows(cpop1,sumpop)


#SDO Housing data
#sdohousmulti <- subset(sdohousedata,placefips %in% uniqmulti)
#sdohousmulti <- sdohousmulti[which(sdohousmulti$countyfips != "999"),]  
#sumhouse <- sdohousmulti %>% group_by(placefips,year) %>% 
#  summarize(localbp = sum(localbp),
#            localdemo = sum(localdemo),
#            localco = sum(localco),
#            localmhc = sum(localmhc)
#            ) 
#sumhouse$countyfips <- "999"
#sumhouse$id <- ""
#sumhouse$areaname <- ""


#sdohousedata <- bind_rows(sdohousedata,sumhouse)

# census Housing data

# removing county totals ctf_construction
housedata2 <- housedata %>%
         filter(placefips != "00000") %>%
         select(countyfips, placefips, id, year, censusbp, censushu)
chouse <- housedata2

# extracting multi-county places and summarizing

#chousemulti <- subset(chouse, placefips %in% uniqmulti)  # this is the list of contains the multi county places for summarization

#sumhouse <- chousemulti %>% group_by(placefips,year) %>% 
#  summarize(censusbp = sum(censusbp),
#            censushu = sum(censushu)) 
#sumhouse$countyfips <- "999"
#sumhouse$id <- ""
#sumhouse <- sumhouse[,c(5,1,6,2:4)]

#chousedata <- bind_rows(chouse,sumhouse)
chousedata <- chouse

# Now have 4 data sets:
# 1) sdopopdata: SDO population estimates, long key vars: countyfips, placefips vartype
# 2) cpopdata: Census population estimates, long key vars: countyfips, placefips vartype
# 3) sdohousedata:  SDO building permit/housing estimates, long key vars: countyfips, placefips year
# 4) chousedata:  Census building permit/housing estimates, long key vars: countyfips, placefips year

# Creating wide SDO Population data set


sdopop <- sdopopdata %>% select(id, countyfips, placefips, vartype, tp, groupquarters, hp, 
                                thu, tp, ohu, pph, vhu, vr) %>%
          mutate(tp =  formatC(tp, format="d", big.mark=","),
                groupquarters =  formatC(groupquarters, format="d", big.mark=","),
                hp =  formatC(hp, format="d", big.mark=","),
                thu =  formatC(thu, format="d", big.mark=","),
                ohu	= formatC(ohu,format="d",big.mark=","),
                pph	= format(pph,trim=TRUE),
                vhu	= formatC(vhu,format="d",big.mark=","),
                vr = format(vr,trim=TRUE)
          )

sdopop2 <- sdopop %>% gather(popname,val, -id, -countyfips, -placefips, -vartype) %>% filter(placefips != "00000") 
sdopop3 <- unique(sdopop2) %>%  spread(vartype, val) 


#setting variable values Need to update 
sdopop3$popname <- ifelse(sdopop3$popname == "tp","Total Population",
                   ifelse(sdopop3$popname == "groupquarters","Group Quarters Population",
                   ifelse(sdopop3$popname == "hp","Household Population",
                   ifelse(sdopop3$popname == "thu","Total Housing Units",
                   ifelse(sdopop3$popname == "ohu","Occupied Housing Units",
                   ifelse(sdopop3$popname =="pph", "Persons per Household",
                   ifelse(sdopop3$popname == "vhu", "Vacant Housing Units", "Vacancy Rate")))))))
                          
                         
sdopop3$popname <- factor(sdopop3$popname, levels = c("Total Population",
                                                      "Group Quarters Population",
                                                      "Household Population",
                                                      "Total Housing Units",
                                                      "Occupied Housing Units",
                                                      "Vacant Housing Units",
                                                      "Vacancy Rate",
                                                      "Persons per Household"))



# cpopdata 

cpop1 <- cpopdata %>% mutate(censuspop = formatC(censuspop, format="d", big.mark=",")) %>%
       select(countyfips, placefips, id, year, vartype, censuspop)


cpop2 <- cpop1 %>% gather(popname,val, -id, -countyfips, -placefips, -vartype) 
cpop3 <- unique(cpop2) %>%  spread(vartype, val)

cpop3$popname <- "Total Population"


#SDO Housing   

sdobp <- sdohousedata %>% select(id, countyfips, placefips,  year, localbp, localdemo, localco)


sdobp$id <- paste0(sdobp$countyfips,sdobp$placefips)
sdobp$localbp <- formatC(sdobp$localbp, format="d", big.mark=",")
sdobp$localdemo <- formatC(sdobp$localdemo, format="d", big.mark=",")
sdobp$localco <- formatC(sdobp$localco, format="d", big.mark=",")


sdobp2 <- sdobp %>% gather(popname,val, -id, -countyfips, -placefips, -year) %>% filter(placefips != "00000") 
sdobp3 <- unique(sdobp2) %>%  spread(year, val)


sdobp3$popname <- ifelse(sdobp3$popname == "localbp","Local Building Permits (issued six months prior to date)",
                  ifelse(sdobp3$popname == "localdemo","Local Demolition Permits",
                  ifelse(sdobp3$popname == "localco","Local Certificates of Occupancy","Local Mobile Home Change")))

sdobp3$popname <- factor(sdobp3$popname, levels = c("Local Building Permits (issued six months prior to date)","Local Certificates of Occupancy",
                         "Local Demolition Permits", "Local Mobile Home Change"))


# Census Housing  
 cbp <- chousedata %>% select(id, countyfips, placefips, year, censusbp, censushu)
 cbp$id <- paste0(cbp$countyfips,cbp$placefips)
 
 
 cbp2 <- cbp %>% gather(popname,val, -id, -countyfips, -placefips, -year) 
 cbp2$val <- formatC(cbp2$val, format="d", big.mark=",")
 cbp2a <- cbp2 %>% filter(popname == "censusbp") %>% filter(countyfips != "999")
 cbp2b <- cbp2 %>% filter(popname == "censushu") %>% filter(countyfips != "999")
 cbp2multia <- cbp2 %>% filter(popname == "censusbp") %>% filter(countyfips == "999") 
 cbp2multib <- cbp2 %>% filter(popname == "censushu") %>% filter(countyfips == "999") 
 
 
 cbp3a <- cbp2a %>% spread(year, val) 
 cbp3b <- cbp2b %>% spread(year, val)   
 

 cbp3multia <- cbp2multia %>%  group_by_at(vars(-val)) %>%  # group by everything other than the value column. 
   mutate(row_id=1:n()) %>% ungroup() %>%  # build group index
   spread(key=year, value=val)    # spread
 
 
 cbp3multib<- cbp2multib %>%  group_by_at(vars(-val)) %>%  # group by everything other than the value column. 
   mutate(row_id=1:n()) %>% ungroup() %>%  # build group index
   spread(key=year, value=val)   
 
 cbp3 <- bind_rows(cbp3a, cbp3b, cbp3multia, cbp3multib) %>% arrange(id, popname)
 
 cbp3 <- cbp3 %>% select(-row_id)
 
 cbp3$popname <- ifelse(cbp3$popname == "censushu","Census Housing Units","Census Building Permits (issued six months prior to date)")
 cbp3$popname <- factor(cbp3$popname, levels=c("Census Housing Units","Census Building Permits (issued six months prior to date)"))
 
 
