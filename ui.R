
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
      titlePanel("How unique and rare are the birds on your checklist?"),
      mainPanel(
        h4("ED and EDGE scores quantify uniqueness and rarity"),
        p(""), 
        p(strong("Evolutionarly Distinctivness (ED Score)"),"is a measure of how unique a species is based on the number and distance of living relatives. A recently diverged species that has many close relatives,such as most gulls (blue branches in the diagram), has low Evolutionary Distinctiveness. Species that diverged early in their evoultionary history and have few (or no) living relatives such as the Hoatzin (red branch), have a high ED score"),
        p(strong("EDGE Score"),"combines the",strong("ED"),"score,with how",strong("G"),"lobally",strong("E"),"ndangered, a species is. The latter is determined by the IUCN Red List of threatened species."), 
        p("Abundant species with many close relatives have", strong("lowest EDGE scores"),",while critically endangered species that have few (or no) close relatives have the",strong("highest EDGE scores.")), 
        p("Species with high EDGE scores are often hard to find, and extremely unusual in the way they look or behave, making some of the most memorable lifers a birder can observe!"),
        p("This web app allows you to determine which species on your life list have the highest EDGE scores. You can do this by uploading your eBird checklist."),
        p("You can use the Explore tab to explore the global list of 9,993 bird species."),  
        p("If you don’t have an ebird checklist, you can still determine your highest EDGE sightings using the this tab")
          )),    
        br(),br(),
    
    ### New tab
    
    tabPanel(
      "Upload your eBird checklist",
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
          checkboxInput('pd', 'Calculate PD? (slow, wait a minute or so)', value = FALSE)
          # checkboxInput('allbirds', 'Browse all bird species? (slow, only works if .csv file hasn\'t been loaded)', value = FALSE),
          # hr(),
        ),
        mainPanel(
          titlePanel("ED and EDGE scores"),
          h3("Summary stats"),
          tableOutput('summary.tab'),
          h3("Details by species"),
          DT::dataTableOutput('phylo.results')
        )
      )
    ),
    
    ### New tab
    
    tabPanel(
      "Explore",
      sidebarLayout(
        sidebarPanel(
          titlePanel("In this Tab you can:", windowTitle = ""),
          p(strong("Search"), "for individual species using the search box"),
          p(strong("Sort"), "species by their scores, rank or names using the small arrows on top of each column"), 
          p(strong("Filter"), "species species using the boxes on top of each column"),
          p(strong("Create"), "your own EDGE checklist by clicking on species"),
          p(strong("Dowload"), "your own EDGE cheklist"),
          p("Get",strong("PD"),"score of your checklist"),
          p(" "),
          downloadButton("downloadData", "Download my checklist"),
          p(" "),
          actionButton("action", "Get PD score(slow, wait a minute...)"),
          p(""),
          p("This means that the combined phylogenetic distance of all the species in your checklist encompasses"),
          p(verbatimTextOutput("pd2", placeholder = FALSE),"Million years of evolution!")),
        mainPanel(
          titlePanel(
            "Explore the complete EDGE bird list"
          ),
          DT::dataTableOutput("origTable"),
          DT::dataTableOutput("origTableSelected")
        )
      )
    ),
    
    ### New tab
    
    tabPanel("Help the birds!",
             sidebarLayout(
               sidebarPanel(width=4,
                 titlePanel("Join the EDGE challenge", windowTitle = ""),
                  p("You can help the bird by donating the sum of the top 5 EDGE scores on your checklist. Click one of the buttons below to find out your score"),
                   actionButton("action3", "eBird checklists",style="width:235px"),
                   actionButton("action2", "Manually entered checklists",style="width:235px"),
                 verbatimTextOutput("EDSCORE1", placeholder = FALSE),
                 verbatimTextOutput("EDSCORE2", placeholder = FALSE),
                 br(),br(),
               p("More about EDGE scores:", a(href="http://edgeofexistence.org/birds/default.php",strong("here."))),
               br(),
               p("More about this app and ED scores:",a(href="http://sebpardo.github.io/ebirdtrees/",strong("here.")))),
             mainPanel(
               p("EDGE score is a tool to prioritize conservation effort directed towards the world’s most unique and rare birds. Your support of the London Zoological Society will help to fund",a(href="http://edgeofexistence.org/conservation/current_projects.php",strong("grassroots")),"new conservation projects directed towards the world’s most unique and most threatened birds, so future generations get a chance to add them to their list as well!"),
               p("Donate", a(href="http://edgeofexistence.org/support/donation_form.php?donationType=single&causeID=0", strong("here."))),
               img(src="GiantIbis.jpeg",height="100%",width="100%"),
               p("The Giant Ibis,", em("Thamantibis gigantea"), "or", em("Pseudibis gigantea")," depending on who you ask, is the bird species with the highest EDGE score. Illustration by Henrik Grönvold - 1911 / Public Domain")
)))
    )))
