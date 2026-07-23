
# Revision to CTF program Adam Bickford June 2026 Vintage 2025
# There are many changes, including updates to tidyverse,
# addition of code to read data from the postgres database,
# and update of the data processing steps to read the new
# data tables.

# This file does the table processing...

library(knitr)
library(kableExtra)
source("setup.R")

# Pop Tab is State Demography Office Population Estimates and Census Population and Housing Estimates
pop_tab <- function(indata,capstr) {

#Kable population table
  popname <- names(indata)
  inmat <- as.matrix(indata)

#Creating Column Names  for the population estimates table
  popname[1] <- "Variable"
  for(i in 2:length(popname)){
    popname[i] <- paste0("July ", popname[i])
  }
 
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

#bp_tab is the building permits Tab State Demography Office Building Permit Estimates
# 
bp_tab <- function(indata,capstr){

  housename <- names(indata)
  inmat <- as.matrix(indata)


  #Creating Column Names  
  housename[1] <- "Variable"
  for(i in 3: length(housename)){
    housename[i] <- paste0(as.numeric(unlist(housename[i]))-1," to ", as.numeric(unlist(housename[i])))
  }

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

  m.sdopop <- sdopop[c(6, 1, 2, 5, 3, 7, 8, 4),4:ncol(sdopop)]  
  m.cpop <- cpop[1,4:ncol(cpop)]
  m.sdobp <- sdobp[c(1:3),4:ncol(sdobp)]  
  m.cbp <- cbp[c(2,1),4:ncol(cbp)]  
  m.cpop <- bind_rows(m.cpop, m.cbp)

  m.sdopop[,2:ncol(m.sdopop)] <- sapply(m.sdopop[,2:ncol(m.sdopop)], function(x) gsub("NA","",x))
  m.sdobp[,2:ncol(m.sdobp)] <- sapply(m.sdobp[,2:ncol(m.sdobp)], function(x) gsub("NA","",x))
  m.cpop[,2:ncol(m.cpop)] <- sapply(m.cpop[,2:ncol(m.cpop)], function(x) gsub("NA","",x))
  
  sdopoptab <-  pop_tab(m.sdopop,"<b><u>State Demography Office Population Estimates</u></b>")
  cpoptab <- pop_tab(m.cpop,"<b><u>U.S. Census Bureau Population and Housing Estimates</u></b>")
  
  sdobptab <-  bp_tab(m.sdobp,"<b><u>State Demography Office Housing Estimates</u></b>")
 
  
#  outtab <- rbind(sdopoptab,sdobptab,cbptab)
  outtab <- rbind(sdopoptab,sdobptab,cpoptab)  
  return(outtab)
}

tab_process <- function(plnum,ctymat,sdopop,censpop,sdobp,censbp) {
  #Function to process output tables data, retuns tabPanels for display

 
  if(plnum == "99990")  { # Unincoprorated area
    idval <- paste0(ctymat[1,1],plnum)
    sdopop <- subset(sdopop, id %in% idval)
    cpop <- subset(censpop, id %in% idval)
    sdobp <- subset(sdobp, id %in% idval)
    cbp <-  subset(censbp, id %in% idval)
  }  else {
    sdopop <- subset(sdopop, placefips %in% plnum)
    cpop <- subset(censpop, placefips %in% plnum)
    sdobp <- subset(sdobp, placefips %in% plnum)
    cbp <- subset(censbp,  placefips %in% plnum)
  }
  # if single county or multiple county   
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
  selPlace <-  areas %>% filter(place == plName) %>% arrange(county)
  
  plnum <- selPlace[1,5]
  ctyMat <- as.matrix(selPlace[,c(4,3)])  

   pop_tabl <- tab_process(plnum,ctyMat,sdopop3,cpop3,sdobp3,cbp3)
   output$sdo=renderUI({do.call(tabsetPanel,pop_tabl)}) 
  })
 }
