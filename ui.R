
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
      sidebarPanel(img(src="edgetree2.png",height="100%",width="100%")),
      titlePanel("Find out how rare and unique are the birds on your checklist with EDGE score!"),
      mainPanel(
        h4("How is EDGE score calculated?"),
        p("EDGE scores have two components:"), 
        p("1. Evolutionarly Distinctivness (ED Score) is how unique a species is based on the number and distance of living relatives. A recently diverged species that has many close relatives has low Evolutionary Distinctiveness, like gulls (blue branches in the diagram). Species that diverged early in their evoultion history and have few (or no) living relatives have a high ED score, like the Hoatzin (red branch)"),
        p("2. How Globally Endangered a species is determined by population size and level of conservation threat (from the IUCN Red List). Abundant species with many close relatives have low EDGE scores, while critically endangered species that have few (or no) close relatives have the highest scores."), 
        p("Species with high EDGE scores are often hard to find, and extremely unusual in the way they look or behave, making some of the most memorable lifers a birder can observe!"),
        p("This web app allows you to determine which species on your life list have the highest EDGE scores. You can find this out in two different ways. You can explore the full species list and enter your data manually in the first tab or if you have an eBird account, you can use the “Upload your eBird data” tab to obtain EDGE scores directly from eBird."))),  
        br(),br(),
    tabPanel(
      "Upload your eBird data",
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
      "Explore and select species",
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
    ),
    tabPanel("Join the EDGE challenge!",
             mainPanel(
      p("EDGE score is a tool to prioritize conservation effort directed towards the world’s most unique and rare birds. Join the EDGE challenge by donating the sum of the EDGE score of the top 5 EDGE birds on your list. Your support of the London Zoological Society will help to fund grassroots conservation efforts directed towards the world’s most unique and most threatened birds, so future generations get a chance to add them to their list as well!"),
      tags$p("Donate here:"),
      tags$a(href="http://edgeofexistence.org/support/donation_form.php?donationType=single&causeID=0","http://edgeofexistence.org/support/donation_form.php?donationType=single&causeID=0"),
      tags$p("More about EDGE scores:"),
      tags$a(href="http://edgeofexistence.org/birds/default.php","http://edgeofexistence.org/birds/default.php"),
      tags$p("More about this app"),
      tags$a("http://sebpardo.github.io/ebirdtrees/","http://sebpardo.github.io/ebirdtrees/"))) 
       )
     )
   )
