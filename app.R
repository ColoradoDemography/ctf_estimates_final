#' Conservation Trust Fund Redesign
#' @author  Adam Bickford, Colorado State Demography Office, June 2023

rm(list = ls())
library(tidyverse, quietly=TRUE)
library(readr)
library(readxl, quietly=TRUE)
library(scales, quietly=TRUE)
library(codemogAPI, quietly=TRUE)
library(codemogProfile, quietly=TRUE)
library(codemogLib)
library(RPostgreSQL, quietly=TRUE)
library(shiny, quietly=TRUE)
library(shinyjs, quietly=TRUE)

#PopPlace Function returns a list of names,  county codes and place codes

# Structure of user Interface

desc_text <- "Overview:</br>
  <p>The Colorado Department of Local Affairs has created population estimates for use in the 
Conservation trust Program.  The population estimates are the basis for the 
distribution of Conservation Trust Fund (CTF) lottery funds to your jurisdiction. </p>
<p>These estimates are based on the April 1st, 2020 Census plus annual data on births, deaths,
net migration, change in group quarters facilities and housing unit change.  
The estimates for the prvious year will be different than the one released this 
year because of the updated census migration estimate data.</p>"

ui <- fluidPage(
    tags$link(rel = "stylesheet", type = "text/css", href = "www/css/common.css"),  #Link to CSS...
    titlePanel("Draft Conservation Trust Find Population Estimates for Calendar Vintage 2023"),
    tags$html(desc_text),
    
                 ) # dashboardPage/ui


# Server Management Function
server <- function(input, output, session) {
 
  
  # updates Dropdown boxes and selects data level and unit

  PlaceList <- popPlace()
  # Populate the input$level  dropdown 
  observeEvent(input$level, ({
 # Output the data tables here
  }))  #observeEvent input$level
}

