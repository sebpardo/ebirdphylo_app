
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
      titlePanel("Upload file", windowTitle = "ED and EDGE scores"),
      fileInput('mydata', 'Specify location of your eBird data file ("MyEBirdData.csv")', 
                accept=c('text/csv', '.csv')),
      tags$p("You can obtain a .csv file with all your eBird data by clicking on the following link (Make sure you're logged into eBird):"),
      tags$a(href="http://ebird.org/ebird/downloadMyData", 
             "http://ebird.org/ebird/downloadMyData"),
      br(),br(),
      p("An email with a download link will be sent to you shortly after confirming the request (pressing 'Submit') through that link."),
      #hr(),
      checkboxInput('grouping', 'Group by country?', value = FALSE),
            # checkboxInput('allbirds', 'Browse all bird species? (slow, only works if .csv file hasn\'t been loaded)', value = FALSE),
      # hr(),
      sliderInput('edfilter', 'ED Score range', min=0.5, max=73, value = c(0.5,73)),
      #sliderInput('edgefilter', 'EDGE Score range', min=0.5, max=7, value = c(0.5,7))
      sliderInput('edgerankfilter', 'EDGE Rank cutoff', min=1, max=9993, step = 10, value = 1000)
      ),
    mainPanel(
      titlePanel("ED and EDGE scores"),
      tableOutput('phylo.results')
      )
  )))

