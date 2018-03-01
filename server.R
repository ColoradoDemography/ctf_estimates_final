
library(plotly)
source("setup.R")

function(input, output, session) {

  id=reactive({as.numeric(filter(areas, Name==input$area)%>%
                              select(id))})
  
  output$sdo=renderTable({
    sdo%>%
      filter(id==id())%>%
      select(-id)},
    digits = 0, 
    include.rownames=FALSE)
  
  output$contact=renderTable({
    contact%>%
      filter(id==id())%>%
      select(-id)%>%
      gather(Field, Current)}, 
    include.rownames=FALSE
  )
  
  output$housing=renderTable({
    housing%>%
      filter(id==id())%>%
      select(-id)}, 
    digits = 0,
    include.rownames=FALSE
  )
  
  
  output$census=renderTable({
    census%>%
      filter(id==id())%>%
      select(-id)}, 
    digits = 0,
    include.rownames=FALSE
  )

}
