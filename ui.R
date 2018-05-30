library(plotly)
library(shiny)

source("setup.R")  

function(req) {
  htmlTemplate("index.html",
               area=selectInput("area","Select your Area:", choices = unique(areas$place)),               
               sdo_estimates=tableOutput("sdo")
               )
}

