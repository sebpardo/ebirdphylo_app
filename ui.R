
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinythemes)
# up file limit to 30 MB, default is 5 MB
options(shiny.maxRequestSize=30*1024^2) 


shinyUI(fluidPage(tags$style(type="text/css",
                       ".shiny-output-error { visibility: hidden; }",
                       ".shiny-output-error:before { visibility: hidden; }"
  ),
  theme = shinytheme("cerulean"),
  navbarPage(
    "",
    tabPanel(
      strong("About EDGE Scores"),
      absolutePanel(img(src="edgetree3.png",height="28%",width="28%"),top=240,left=20),
      absolutePanel(
        h2(em("Finding the world's most extraordinary species"),align="center"),
        br(),
        br(),
        p("Bird EDGE scores help to prioritize conservation effort towards the world’s most unique and endangered birds.",strong ("Evolutionarly Distinctivness (ED Score)"),"is a measure of how unique a species is based on the number and evolutionary distance of living relatives. A recently diverged species that has many close relatives, such as most gulls (blue branches in the diagram), has low Evolutionary Distinctiveness. Species that diverged early in their evoultionary history and have few (or no) living relatives, such as the Hoatzin (red branch), have a high Evolutionary Disctinctivness."),
        p(strong("EDGE Score"),"combines",strong("Evolutionary Disctinctivness (ED)"),"score, with how",strong("Globally Endangered (GE)"),"a species is. The latter is determined by the IUCN Red List of threatened species."), 
        p("Abundant species with many close relatives have the", strong("lowest EDGE scores"),", while critically endangered species that have few (or no) close relatives have the",strong("highest EDGE scores."),"Species with high EDGE scores are likely be some of the most memorable lifers for any birder, as they often hard to find and extraordinary in the way they look or behave."),
        p("You can find out which species in your life list have the highest EDGE scores using this web app. To do this, you can simply upload your eBird checklist. If you don’t use eBird to keep track of your sightings, you can use the 'Explore' tab, to browse and select from the complete list of 9,993 bird species.")
        ,top=180,left=300,right=15,width="auto",height="auto",style="padding: 24px; border-bottom: 2px solid #; background: #EAECEE;"),    
        absolutePanel(img(src="banner1.png",height="100%",width="100%"),top=51,left=15,right=15),
        absolutePanel(strong("#8"),"Philippine Eagle",align="right",style="color: #FDFEFE;",top=48,right=25),
      absolutePanel("ⒸAlain Pascua",align="right",style="color: #FDFEFE;font-weight: 100",top=68,right=25)
       ),
    
    ### New tab
    
    tabPanel(
      strong("Upload your eBird checklist"),
      sidebarLayout(
       absolutePanel(
          h3("Upload file"),
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
          checkboxInput('pd', 'Calculate the total million years of evolution contained in your checklist? (slow, wait a minute or so)', value = FALSE)
          # checkboxInput('allbirds', 'Browse all bird species? (slow, only works if .csv file hasn\'t been loaded)', value = FALSE),
          # hr(),
        ),
        absolutePanel(
          h2(em("Your EDGE species checklist:"),align="center"),
          tableOutput('summary.tab'),
          DT::dataTableOutput('phylo.results'),
          top=180,left=300,width="auto",height="auto",style="padding: 24px; border-bottom: 2px solid #;"
        )
      ),
      absolutePanel(img(src="banner3.jpg",height="100%",width="100%"),top=51,left=15,right=15),
      absolutePanel(strong("#408"),"Whooping Crane",align="right",style="color: #FDFEFE;",top=48,right=25),
      absolutePanel("Creative Commons",align="right",style="color: #FDFEFE;font-weight: 100",top=68,right=25)
    ),
    ### New tab
    
    tabPanel(
      strong("Explore"),
      sidebarLayout(
        absolutePanel(
          h3("In this tab you can also:"),
          p(strong("Search"), "for individual species using the search box."),
          p(strong("Sort"), "species by their scores, rank, or names using the small arrows on top of each column."), 
          p(strong("Filter"), "species species using the boxes at the top of each column"),
          p(strong("Create"), "your own EDGE checklist by clicking on species"),
          p(strong("Dowload"), "your own EDGE cheklist"),
          p("Get",strong("PD"),"score of your checklist"),
          br(),
          downloadButton("downloadData", "Download my checklist",style="width:246px"),
          br(),
          br(),
          p(strong("Calculate"),"how many million years of evolution are contained in your checklist, using the button below."),
          p(verbatimTextOutput("pd2", placeholder = FALSE),""),
                    actionButton("action", "(slow, wait a minute or so..)"),
          br(),
          top=180,left=15,width=285,height="auto",style="padding: 24px; border-bottom: 2px solid #; background: #EAECEE;"),
        absolutePanel(
          h2(em(
            "Explore the complete EDGE bird list"),align="center"
          ),
          DT::dataTableOutput("origTable"),
          DT::dataTableOutput("origTableSelected"),
          top=180,left=300,width="auto",height="auto",style="padding: 24px; border-bottom: 2px solid #;"
      )
    ),
          absolutePanel(img(src="banner2b.png",height="100%",width="100%"),top=51,left=15,right=15),
      absolutePanel(strong("#4"),"Kakapo",align="right",style="color: #FDFEFE;",top=48,right=25),
      absolutePanel("ⒸShane McInnes",align="right",style="color: #FDFEFE;font-weight: 100",top=68,right=25)
    ),
    ### New tab
    
    tabPanel(
      strong("Help the birds!"),
             sidebarLayout(
               sidebarPanel(
                   tags$head(
                   tags$style(type="text/css", "select { max-width: 280px; }"),
                   tags$style(type="text/css", ".span4 { max-width: 280px; }"),
                   tags$style(type="text/css", ".well { max-width: 280px; }"),
                   tags$style(type="text/css", "select { min-width: 280px; }"),
                   tags$style(type="text/css", ".span4 { min-width: 280px; }"),
                   tags$style(type="text/css", ".well { min-width: 280px; }")
                  ),
                 titlePanel("Join the EDGE challenge", windowTitle = ""),
                  p("You can help the bird by donating the sum of the top 5 EDGE scores on your checklist. Click one of the buttons below to find out your score"),
                   actionButton("action3", "eBird checklists",style="width:239px"),
                   actionButton("action2", "Manually entered checklists",style="width:239px"),
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
