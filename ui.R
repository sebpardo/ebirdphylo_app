
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinythemes)
# up file limit to 30 MB, default is 5 MB
options(shiny.maxRequestSize=30*1024^2) 


shinyUI(fluidPage(
  theme = shinytheme("cerulean"),
  navbarPage(
    "EDGE APP",
    tabPanel(
      "About EDGE Scores",
      titlePanel("What is the EDGE score for birds?"),
      mainPanel(
        "The Evolutionary Distinctive Globally Endangered (EDGE) score of a species is a measure of the rarity of a given bird based not only on each species distribution and level of threat (as indicated by the IUCN Red List), but also on how unique a species is based on the number of alive relatives (that is, its Evolutionary distinctiveness, or ED). Abundant species that have many living close related species will have a low score. A critically endangered species that has few (or no) living relatives will have the highest scores. EDGE score can help to prioritize conservation efforts of the world’s rarest birds.",
br(),br(),
"This web app allows you to obtain EDGE scores for bird species in your eBird checklists, or species entered manually from a list. Please click on one of the tabs above to choose whether to enter your data from eBird or manually.",
br(),br(),
"For more information visit:",br(),br(),
tags$a(href = "http://edgeofexistence.org/birds/default.php",
       "http://edgeofexistence.org/birds/default.php"),
br(),
tags$a(href = "http://sebpardo.github.io/ebirdtrees/",
       "http://sebpardo.github.io/ebirdtrees/")
    )),
    
    tabPanel(
      "Via eBird",
      sidebarLayout(
        sidebarPanel(
          titlePanel("Upload file", windowTitle = "ED and EDGE scores"),
          fileInput(
            'mydata',
            'Specify location of your eBird data file ("MyEBirdData.csv")',
            accept = c('text/csv', '.csv')
          ),
          tags$p(
            "You can obtain a .csv file with all your eBird data by clicking on the following link (Make sure you're logged into eBird):"
          ),
          tags$a(href = "http://ebird.org/ebird/downloadMyData",
                 "http://ebird.org/ebird/downloadMyData"),
          br(),
          br(),
          p(
            "An email with a download link will be sent to you shortly after confirming the request (pressing 'Submit') through that link."
          ),
          #hr(),
          checkboxInput('grouping', 'Group by country?', value = FALSE),
          checkboxInput('sort_ed', 'Sort species by ED Score?', value = FALSE),
          checkboxInput('pd', 'Calculate PD? (slow, wait a minute or so)', value = FALSE),
          # checkboxInput('allbirds', 'Browse all bird species? (slow, only works if .csv file hasn\'t been loaded)', value = FALSE),
          # hr(),
          sliderInput(
            'edfilter',
            'ED Score range',
            min = 0.5,
            max = 73,
            value = c(0.5, 73)
          ),
          #sliderInput('edgefilter', 'EDGE Score range', min=0.5, max=7, value = c(0.5,7))
          sliderInput(
            'edgerankfilter',
            'EDGE Rank cutoff',
            min = 100,
            max = 10000,
            step = 100,
            value = 1000
          )
        ),
        mainPanel(
          titlePanel("ED and EDGE scores"),
          h3("Summary stats"),
          tableOutput('summary.tab'),
          h3("Details by species"),
          tableOutput('phylo.results')
        )
      )
    ),
    
    tabPanel(
      "Manually select species",
      sidebarLayout(
        sidebarPanel(
          titlePanel("2. EDGE score calculator", windowTitle = ""),
          actionButton("action", "Sum of my top 5 EDGE species:"),
          verbatimTextOutput("EDSCORE", placeholder = FALSE)
          ,
          p("3. dowload a list of the selected species"),
          downloadButton("downloadData", "Download my list")
        ),
        mainPanel(
          titlePanel(
            "1. Manually select species from the list below"
          ),
          DT::dataTableOutput("origTableSelected"),
          DT::dataTableOutput("origTable")
        )
      )
    )
  )
))
