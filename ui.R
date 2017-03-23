
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
                accept=c('text/csv', '.csv')),
      tags$hr(),
      p("You can obtain a .csv file with all your eBird data by clicking on the following link (Make sure you're logged into eBird):"),
      tags$a(href="http://ebird.org/ebird/downloadMyData", "http://ebird.org/ebird/downloadMyData"),
      br(),br(),
      p("An email with a download link will be sent to you shortly after confirming the request (pressing 'Submit') through that link.")
      ),
    mainPanel(
      titlePanel("ED and EDGE scores (top 1000 species)"),
      tableOutput('phylo.results')
      )
  )))

