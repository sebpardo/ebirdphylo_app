
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dplyr)
#devtools::install_github('sebpardo/myebird')
library(myebird)

function(input, output) {
  output$phylo.results <- renderTable({
  if(!is.null(input$mydata)) {
    mydata <- ebirdclean(input$mydata$datapath)
  edge <- myedge(mydata, edge.cutoff = 1000) %>% select(-sciName.edge)
  return(edge)
  } else {
    return(data.frame(NULL))
  }
  })
}
