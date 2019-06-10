
# Revision to CTF program Adam Bickford June 2019
# There are many changes, including updates to tidyverse,
# addition of code to read data from the postgres database,
# and update of the data processing steps to read the new
# data tables.

# This file does the table processing...

library(knitr)
library(kableExtra)
source("setup.R")



pop_tab <- function(indata,capstr) {

#Kable population table
  popname <- names(indata)
  inmat <- as.matrix(indata)
  
#Creating Column Names  
  popname[1] <- "Variable"
  popname[2] <- "2010 Census"
  popname[3] <- "2010 Adj. Census"
  popname[4:length(popname)] <- sapply(popname[4:length(popname)], function(x) paste0("July ",x))
  
  
  outtab <- inmat %>%
  kable(format='html',
        row.names=FALSE,
        align= c("l",rep("r",ncol(indata)-1)),
        col.names = popname,
        caption = capstr,
        escape = FALSE)  %>%
    kable_styling() %>%
    row_spec(0, align = "c") %>%
    column_spec(1, width="2in") %>%
    column_spec(2:ncol(indata), width="0.5in") 
    
 return(outtab)
}

bp_tab <- function(indata,capstr){
  housename <- names(indata)
  inmat <- as.matrix(indata)
  
 
  #Creating Column Names
  housename[1] <- "Variable"
  housename[2:length(housename)] <- sapply(housename[2:length(housename)], function(x) paste0(as.numeric(x)-1," to ",as.numeric(x)))
  
  outtab <- inmat %>%
    kable(format='html',
          row.names=FALSE,
          align=c("l",rep("r",ncol(indata)-1)),
          col.names = housename,
          caption = capstr,
          escape = FALSE)  %>%
    kable_styling() %>%
    row_spec(0, align = "c") %>%
    column_spec(1, width="3.5in") %>%
    column_spec(2:ncol(indata), width="0.5in") 
    
  return(outtab)
}


tab_proc <- function(sdopop,cpop,sdobp,cbp) {
 #Function that creates combined population and housing tables

  m.sdopop <- sdopop[c(6,1,2,4,5,3,7,8),4:ncol(sdopop)]
  m.cpop <- cpop[,4:ncol(cpop)]
  m.sdobp <- sdobp[,c(4,6:ncol(sdobp))]
  m.cbp <- cbp[c(2,1),c(4,6:ncol(cbp))]
  
  sdopoptab <-  pop_tab(m.sdopop,"State Demography Office Population Estimates")
  cpoptab <- pop_tab(m.cpop,"U.S. Census Bureau Population Estimates")
  
  sdobptab <-  bp_tab(m.sdobp,"State Demography Office  Housing Estimates")
  cbptab <- bp_tab(m.cbp,"U.S. Census Bureau Housing Estimates")
  
#  outtab <- rbind(sdopoptab,sdobptab,cbptab)
  outtab <- rbind(sdopoptab,cpoptab,sdobptab,cbptab)  
  return(outtab)
}

tab_process <- function(plnum,ctymat,sdopop,censpop,sdobp,censbp) {
  #Function to process output tables data, retuns tabPanels for display
 
  if(plnum == "99990")  { # Unincoprorated area
    idval <- paste0(ctymat[1,1],plnum)
    sdopop <- subset(sdopop, id %in% idval)
    cpop <- subset(censpop, id %in% idval)
    sdobp <- subset(sdobp, id %in% idval)
    cbp <- subset(censbp, id %in% idval)
    }  else {
    sdopop <- subset(sdopop, placefips %in% plnum)
    cpop <- subset(censpop, placefips %in% plnum)
    sdobp <- subset(sdobp, placefips %in% plnum)
    cbp <- subset(censbp, placefips %in% plnum)
    }
  
 if(nrow(sdopop) == 8) { # Single county Municipality
    outtab <- tab_proc(sdopop,cpop,sdobp,cbp) 
    outlist <- list(tabPanel(ctymat[1,2],HTML(outtab)))
   return(outlist)
 } else { # multicounty cities
   ctynames <- matrix(nrow=nrow(sdopop)/8)
   
   #Generating total
   sdopop2 <- subset(sdopop, countyfips == "999")
   cpop2 <- subset(cpop, countyfips == "999")
   sdobp2 <- subset(sdobp, countyfips == "999")
   cbp2 <- subset(cbp, countyfips == "999")
   
   ctynames[1] = "Total"
   outtab <- tab_proc(sdopop2,cpop2,sdobp2,cbp2)
   outlist <- list(outtab)
   for(i in 1:nrow(ctymat)) {
     ctynames[i+1] = ctymat[i,2]
     sdopop2 <- subset(sdopop, countyfips == ctymat[i,1])
     cpop2 <- subset(cpop, countyfips == ctymat[i,1])
     sdobp2 <- subset(sdobp, countyfips == ctymat[i,1])
     cbp2 <- subset(cbp, countyfips == ctymat[i,1])  
     outtab <- tab_proc(sdopop2,cpop2,sdobp2,cbp2)
     outlist[[i+1]] <- outtab
   } # for
   tablist <- lapply(1:nrow(ctynames), function(i) {
     tabPanel(ctynames[i],HTML(outlist[[i]]))
   })
   return(tablist)   
   } # multicounty cities

}


function(input, output, session) {
  observeEvent(input$area, {
  plNm <- reactive(input$area)
  plName <- as.character(isolate(plNm()))

  # Selecting area records
  selPlace <-  areas[which(areas$place == plName),]
  selPlace <- selPlace[order(selPlace$county),]
  plnum <- selPlace[1,5]
  ctyMat <- as.matrix(selPlace[,c(4,3)])

   pop_tabl <- tab_process(plnum,ctyMat,sdopop3,cpop3,sdobp3,cbp3)
   output$sdo=renderUI({do.call(tabsetPanel,pop_tabl)}) 
  })
 }
