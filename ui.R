
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
        p("This web app allows you to determine which species on your life list have the highest EDGE scores.  
           You can find out in two different ways. 
           If you have an eBird account, you can use the “Upload your eBird data” tab to obtain EDGE scores directly from your eBird checklist. 
           If you don’t have an ebird checklist, go to the “Explore and select species tab”, where you can select species form a list to create your own EDGE checklist"),
         p("You can also calculate the Phylogenetic Distance of your checklist (PD). PD is the sum of all the tree branches (in Million Years), in the tree created by your checklist. The red and blue branches in the diagram, if your checklist only has the Hoatzin and two gulls."))),  
        br(),br(),
    
    ### New tab
    
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
    
    ### New tab
    
    tabPanel(
      "Explore and select species",
      sidebarLayout(
        sidebarPanel(
          titlePanel("", windowTitle = ""),
          p("Use the little triangles on top of each column to sort by name, rank, ED score etc."), 
          p("Search and filter species usinng the boxes on top of each column"),
          p("Create your EDGE checklist by clicking on species"),
          p("You can dowload your EDGE cheklist with the button below"),
          p(" "),
          downloadButton("downloadData", "Download my checklist"),
          p(" "),
          actionButton("action", "Get PD score(slow, wait a minute...)"),
          p(""),
          p("This means that the combined phylogenetic distance of all the species in your checklist encompasses"),
          p(verbatimTextOutput("pd2", placeholder = FALSE),"Million years of evolution!")),
        mainPanel(
          titlePanel(
            "Explore the bottom table and click on species to create your EDGE checklist"
          ),
          DT::dataTableOutput("origTableSelected"),
          DT::dataTableOutput("origTable")
        )
      )
    ),
    
    ### New tab
    
    tabPanel("Help the birds!",
             sidebarLayout(
               sidebarPanel(width=4,
                 titlePanel("Join the EDGE challenge", windowTitle = ""),
                 p("You can help the bird by donating the sum of the top 5 EDGE scores on your checklist. Click one of the buttons below to find out your score"),
                 actionButton("action2", "Manually entered checklists"),
                 actionButton("action3", "eBird checklists"),
                 verbatimTextOutput("EDSCORE1", placeholder = FALSE),
                 verbatimTextOutput("EDSCORE2", placeholder = FALSE),
                 br(),br(),
               p("More about EDGE scores:", a(href="http://edgeofexistence.org/birds/default.php",strong("here."))),
               br(),
               p("More about this app and ED scores:",a(href="http://sebpardo.github.io/ebirdtrees/",strong("here.")))),
             mainPanel(
               p("EDGE score is a tool to prioritize conservation effort directed towards the world’s most unique and rare birds. Your support of the London Zoological Society will help to fund grassroots conservation efforts directed towards the world’s most unique and most threatened birds, so future generations get a chance to add them to their list as well!"),
               p("Donate", a(href="http://edgeofexistence.org/support/donation_form.php?donationType=single&causeID=0", strong("here."))),
               img(src="GiantIbis.jpeg",height="100%",width="100%"),
               p("The Giant Ibis (Thamantibis gigantea or Pseudibis gigantea, depending on who you ask) is the bird species with the highest EDGE score. Illustration by Henrik Grönvold - 1911 / Public Domain")
)))
    )))
