
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
# up file limit to 30 MB, default is 5 MB
options(shiny.maxRequestSize=30*1024^2) 

shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      titlePanel("Upload file"),
      fileInput('mydata', 'Specify location of your eBird data file ("MyEBirdData.csv")', 
                accept=c('text/csv', '.csv'))
      ),
    mainPanel(
      titlePanel("ED and EDGE scores (top 1000 species)"),
      tableOutput('phylo.results')
      )
  
  )))

