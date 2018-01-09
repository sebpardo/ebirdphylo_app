
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
  navbarPage(collapsible=TRUE,
    "eEDGEbirds",
    tabPanel(
      strong("About EDGE Scores"),
      absolutePanel(img(src="edgetree3.png",height=420,width="auto"),top=240,left=15,bottom=40,right=500),
      absolutePanel(
        h2(em("Finding the world's most extraordinary birds"),align="center"),
        br(),
        br(),
        p("Bird EDGE scores help to prioritize conservation effort towards the world’s most unique and endangered birds.",strong ("Evolutionary Distinctiveness (ED Score)"),"is a measure of how unique a species is based on the number and evolutionary distance of living relatives. A recently diverged species that has many close relatives, such as most gulls (blue branches in the tree on the left), has low Evolutionary Distinctiveness. Species that diverged early in their evolutionary history and have few (or no) living relatives, such as the Hoatzin (red branch), have a high Evolutionary Distinctiveness."),
        p(strong("EDGE Score"),"combines the",strong("Evolutionary Distinctiveness (ED)"),"score with how",strong("Globally Endangered (GE)"),"a species is based on the", 
          a(href="http://www.iucnredlist.org/", target="_blank",strong("IUCN Red List of Threatened Species.")),"Abundant species with many close relatives have the", strong("lowest EDGE scores"),"while critically endangered species that have few (or no) close relatives have the",strong("highest EDGE scores."),"Species with high EDGE scores are likely some of the most memorable lifers for any birder, as they are often hard to find and extraordinary in the way they look or behave."),
        p("You can find out which species in your life list have the highest EDGE scores using this web app. To do this, you can upload your eBird checklist. If you do not use eBird to keep track of your sightings, you can use the 'Explore' tab, to browse and select from the complete list of 9,993 bird species.")
        ,top=180,left=320,right=15,width="auto",height="auto",style="min-width:500px;max-width:1000px; padding:24px; border-bottom: 2px solid #; background: #EAECEE;"),    
 absolutePanel(img(src="backbannerD.jpg",height="100%",width="100%",style="min-height:70px;min-width:500px;max-height:164px;
"),top=53,right=15,left=80),
             absolutePanel("ED Score:",strong("32.47"),align="center",top=55,right=22,style="color: #FDFEFE;"),
             absolutePanel("UICN Status:",align="center",top=75,right=22,style="color: #FDFEFE;"),
             absolutePanel(strong("Least Concern"),align="center",top=95,right=22,style="color: #7FFF00;"),
             absolutePanel(img(src="banner8.jpg",height="100%",width="100%",style="min-height:70px;min-width:500px;max-height:165px;max-width:1305px;
"),top=53,left=15,right=15),
             absolutePanel(strong("#894"),"Red-tailed Tropicbird",align="right",top=55,left=22,style="color: #FDFEFE;"),
              absolutePanel("Ⓒ Simon Valdez-Juarez",align="right",style="color: #FDFEFE;font-weight: 100",top=75,left=22)
           ),
    
    ### New tab
    
    tabPanel(
      strong("Upload my eBird checklist"),
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
          tags$a(href = "http://ebird.org/ebird/downloadMyData", target="_blank",
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
          ,top=180,left=15,width=300,height="auto",style="padding: 24px; border-bottom: 2px solid #; background: #EAECEE;"),
        absolutePanel(
          h2(em("My EDGE species checklist:"),align="center"),
          tableOutput('summary.tab'),
          DT::dataTableOutput('phylo.results'),
          top=180,left=300,width="auto",height="auto",style="padding: 24px; border-bottom: 2px solid #;"
        )
      ),
 absolutePanel(img(src="backbannerD.jpg",height="100%",width="100%",style="min-height:70px;min-width:500px;max-height:164px;"
                  ),top=53,right=15,left=80),
             absolutePanel("ED Score:",strong("17.88"),align="center",top=55,right=22,style="color: #FDFEFE;"),
             absolutePanel("UICN Status:",align="center",top=75,right=22,style="color: #FDFEFE;"),
             absolutePanel(strong("Vulnerable"),align="center",top=95,right=22,style="color: #FFFF00;"),
             absolutePanel(img(src="banner9inv.jpg",height="100%",width="100%",style="min-height:70px;min-width:500px;max-height:165px;max-width:1305px;
                               "),top=53,left=15,right=15),
             absolutePanel(strong("#290"),"Hooded plover",align="right",top=55,left=22,style="color: #FDFEFE;"),
             absolutePanel("Ⓒ Alice Domalik",align="right",style="color: #FDFEFE;font-weight: 100",top=75,left=22)
             ),
    ### New tab
    
    tabPanel(
      strong("Explore"),
      sidebarLayout(
        absolutePanel(
          h3("With this tab you can:"),
          p(strong("Search"), "for individual species using the search box"),
          p(strong("Sort"), "species by their scores, rank, or names using the small arrows on top of each column"), 
          p(strong("Filter"), "species using the boxes at the top of each column"),
          p(strong("Create"), "your EDGE checklist by clicking on species"),
          p(strong("Download"), "your EDGE checklist"),
          p("Get",strong("PD"),"score of your checklist"),
          br(),
          downloadButton("downloadData", "Download my checklist",style="width:246px"),
          br(),
          br(),
          p(strong("Calculate"),"how many million years of evolution are contained in your checklist, using the button below."),
          p(verbatimTextOutput("pd2", placeholder = FALSE),""),
                    actionButton("action", "(slow, wait a minute or so..)"),
          br(),
          top=180,left=15,width=300,height="auto",style="padding: 24px; border-bottom: 2px solid #; background: #EAECEE;"),
        absolutePanel(
          h2(em(
            "Explore the complete EDGE bird list"),align="center"
          ),
          DT::dataTableOutput("origTable"),
          DT::dataTableOutput("origTableSelected"),
          top=180,left=300,width="auto",height="auto",style="padding: 24px; border-bottom: 2px solid #;"
      )
    ),
             absolutePanel(img(src="backbannerD.jpg",height="100%",width="100%",style="min-height:70px;min-width:500px;max-height:164px;
"),top=53,right=15,left=80),
             absolutePanel("ED Score:",strong("19.51"),align="center",top=55,right=22,style="color: #FDFEFE;"),
             absolutePanel("UICN Status:",align="center",top=75,right=22,style="color: #FDFEFE;"),
             absolutePanel(strong("Critically endangered"),align="center",top=95,right=22,style="color: #FF3333;"),
             absolutePanel(img(src="banner7inv.png",height="100%",width="100%",style="min-height:70px;min-width:500px;max-height:165px;max-width:1305px;
                               "),top=53,left=15,right=15),
             absolutePanel(strong("#12"),"Northern Bald Ibis",align="right",top=55,left=22,style="color: #FDFEFE;"),
             absolutePanel("Ⓒ Rafa Benjumea - Ecotono Birding",align="right",style="color: #FDFEFE;font-weight: 100",top=75,left=22)
           ),
    ### New tab
    
    tabPanel(
      strong("Help the birds!"),
absolutePanel(
               h3(em("Join the EDGE Challenge!"),align="left"),
               p("Together we can give future generations a chance enjoy the world's most extraordinary birds! You can help to fund future EDGE fellows by donating to the EDGE program. Use the buttons below to find out the sum of your top 5 EDGE birds scores. After you find your score go", a(href="http://edgeofexistence.org/support/donation_form.php?donationType=single&causeID=0", strong("here")),"to donate."),
               actionButton("action3", "eBird checklists",style="width:239px"),
               actionButton("action2", "Manually entered checklists",style="width:239px"),
               br(),
               verbatimTextOutput("EDSCORE1", placeholder = FALSE),
               verbatimTextOutput("EDSCORE2", placeholder = FALSE),
               br(),
               br(),
               p("Learn", a(href="http://edgeofexistence.org/birds/default.php", target="_blank", strong("more about EDGE scores")), "for birds."),
               p("This App was developed by",a(href="https://www.sfu.ca/biology/people/profiles/svaldez.html", target="_blank", strong("Simón Valdez-Juarez")),"and",a(href="http://sebpardo.github.io/", target="_blank", strong("Sebastián Pardo"))),
               top=510,left=15,right=15,width="auto",height="auto",style="min-width:1005px;max-width:1305px; padding:24px; border-bottom: 2px solid #; background: #EAECEE;"
             ),
             absolutePanel(
               h2(em("Saving the world's most extraordinary birds"),align="center"),
               br(),
               p("The", a(href="http://www.zsl.org/", strong("Zoological Society of London (ZSL)")), "helps to fund ",a(href="http://edgeofexistence.org/conservation/fellows_list.php", target="_blank", strong("grassroots")), "conservation projects directed towards the world’s most unique and most threatened species, like the Philippines National bird, the Philippine Eagle",em("(Pithecophaga jefferyi).")),
               p("The", a(href="http://edgeofexistence.org/birds/species_info.php?id=1948", target="_blank", strong("Philippine Eagle")),"was never common within its small range confined to just four islands of the Philippines. Habitat destruction and hunting further reduced the range and numbers of this critically endangered species."),
               p("EDGE fellow Kahlil Panopio and", a(href="http://www.haribon.org.ph/", strong("Haribon")), "a Philippine NGO, identified remaining critical habitat in Mt.Mingan. Then,worked with local communities and governments to shift attitudes toward conservation of this emblematic bird. Thanks to their efforts, 8,227 hectares of critical habitat will soon be declared a protected area."), 
                top=220,left=330,right=15,width="auto",height="auto",style="min-width:700px;max-width:990px; padding:24px; border-bottom: 2px solid #; background: #EAECEE;"),
             absolutePanel(img(src="Kahil-Eagles3.jpg",height=311,width="auto"),top=220,left=15),
             absolutePanel(img(src="backbannerD.jpg",height="100%",width="100%",style="min-height:70px;min-width:500px;max-height:164px;
                               "),top=53,right=15,left=80),
             absolutePanel("ED Score:",strong("23.64"),align="center",top=55,right=22,style="color: #FDFEFE;"),
             absolutePanel("UICN Status:",align="center",top=75,right=22,style="color: #FDFEFE;"),
             absolutePanel(strong("Critically endangered"),align="center",top=95,right=22,style="color: #FF3333;"),
             absolutePanel(img(src="banner1inv.jpg",height="100%",width="100%",style="min-height:70px;min-width:500px;max-height:165px;max-width:1305px;
                               "),top=53,left=15,right=15),
             absolutePanel(strong("#9"),"Philippine Eagle",align="right",top=55,left=22,style="color: #FDFEFE;"),
             absolutePanel("Ⓒ Alain Pascua",align="right",style="color: #FDFEFE;font-weight: 100",top=75,left=22)
             )
             )
           ))

