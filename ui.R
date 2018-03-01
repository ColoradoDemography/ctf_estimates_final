library(plotly)
library(shiny)

source("setup.R")  

function(req) {
  htmlTemplate("index.html",
               area=selectInput("area","Select your Area:", choices = unique(areas$Name)),               
               contact_info=tableOutput("contact"),
               sdo_estimates=tableOutput("sdo"),
               housing=tableOutput("housing")
               )
}

