
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
  theme = shinytheme("yeti"),
  navbarPage(collapsible=TRUE,
    "EDGE App for eBird", # Don't use header (eg, h4()) here as it doesn't render properly in browser tab description 
    tabPanel(
      ("ABOUT EDGE"),
      absolutePanel(img(src="edgetree4.png",height=463,width="auto"),top=520,left=15,bottom=40,right=500),
      absolutePanel(
        h2(em("Finding the world's most extraordinary birds"), align="center"),
        br(),
        br(),
        h3("Use this app to find out which bird species in your life list have the highest",a(href="https://www.edgeofexistence.org/science/", target="_blank", strong("EDGE scores")), ". If you use eBird to keep track of your sightings, simply upload your eBird data in the \"eBird data\" tab. If you do not use eBird to keep track of your sightings, you can use the \"Explore\" tab to browse and select from the complete EDGE list of 10,960 bird species in the" 
            ,a(href="http://datazone.birdlife.org/species/taxonomy", target="_blank",("BirdLife International")),"taxonomy."),
               h3("EDGE scores are a product of how", strong("Evolutionarily Distinct (ED)"), "and", strong("Globally Endangered (GE)"),"a bird species is. Evolutionary Distinctiveness is a measure of how unique a species is, based on the number of living relatives it has, and its evolutionary distance from these relatives. Global Endangerment status for each species is based on the", 
                  a(href="http://www.iucnredlist.org/", target="_blank",("IUCN Red List of Threatened Species")), "assessments undertaken by BirdLife International, the IUCN Red List Authority for birds."),
              h3("Abundant species that have many close relatives, such as most gulls (blue branches in the tree on the left), have low EDGE scores. Critically Endangered species that have few living relatives, such as the California Condor (red branch), have a high EDGE score."),
              h3("EDGE scores are produced and updated by the",a(href="https://www.edgeofexistence.org/",("EDGE of Existence Programme")),"at the", a(href="http://www.zsl.org/", ("Zoological Society of London (ZSL)")),"to highlight the need to preserve the world’s most unique and endangered species."),
              br(),
               top=362,left=338, width="auto", height="auto", style="padding:24px; border-bottom: 2px solid #; background: #EAECEE;"),    
             absolutePanel(img(src="RTTB_04_2018.png",height="100%",width="100%",style="min-height:296px;min-width:1246px;max-height:318px;max-width:1340px;
                               "),top=43,left=15),
                   absolutePanel(img(src="black_patch.png",height="100%",width="100%",style="min-height:45px;min-width:45px;max-height:45;px;max-width:45px;
                               "),top=0,right=0),
             absolutePanel(h5("App by",a(href="https://www.sfu.ca/biology/people/profiles/svaldez.html", target="_blank",("Simón Valdez-Juarez")),"&",br(),a(href="https://sebpardo.github.io/", target="_blank", ("Sebastián Pardo")),
                              br(), "Source code available on", a(href="https://github.com/sebpardo/ebirdphylo_app", target="_blank","GitHub")),top=1020,left=45, align = "center"),
             absolutePanel(h4(strong("# 915")),align="centre",top=75,left=32,style="color: #FDFEFE;"),
             absolutePanel(h4(strong("Red-tailed Tropicbird")),align="right",top=100,left=32,style="color: #FDFEFE;"),
             absolutePanel(h4(em("Phaethon rubricauda")),align="right",style="color: #FDFEFE;font-weight: 100",top=125,left=32),
             absolutePanel(h4("EDGE Score:      3.51"),align="right",style="color: #FDFEFE;font-weight: 100",top=170,left=32),
             absolutePanel("Ⓒ Simon Valdez-Juarez",align="right",style="color: #FDFEFE;font-weight: 100",top=315,left=270),
             absolutePanel(h6(strong("Evolutionary")),top=245,left=26,style="color: #FDFEFE;"),
             absolutePanel(h6(strong("Distinctiveness:")),top=265,left=18,style="color: #FDFEFE;"),
             absolutePanel(h5(strong("32.47")),top=290,left=41,style="color: #FDFEFE;"),
             absolutePanel(h6(strong("Globally")),top=245,left=153,style="color: #FDFEFE;"),
             absolutePanel(h6(strong("Endangered:")),top=265,left=143,style="color: #FDFEFE;"),
             absolutePanel(h6(strong("Least Concern")),align="center",top=290,left=139,style="color: #7FFF00;")             
           ),
    
    ### New tab
    
    tabPanel(
      ("eBIRD DATA"),
      sidebarLayout(
       absolutePanel(
          h3("Upload file"),
         br(),
         br(),
          fileInput(
            'mydata',
            'Specify location of your eBird data file ("MyEBirdData.csv")',
            accept = c('text/csv', '.csv')
          ),
          tags$p(
            "You can obtain a .csv file with your eBird data by clicking on the following link (Make sure you're logged into eBird):"
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
          ,top=332,left=15,width=320,height="auto",style="padding: 24px; border-bottom: 2px solid #; background: #EAECEE;"),
        absolutePanel(
          h2(em("My eBird life list"), align="center"),
          tableOutput('summary.tab'),
          DT::dataTableOutput('phylo.results'),
          top=332,left=338,width="auto",height="auto",style="padding: 24px; border-bottom: 2px solid #;"
        )
      ),
             absolutePanel(img(src="HOPL_04_2018.png",height="100%",width="100%",style="min-height:296px;min-width:1246px;max-height:318;px;max-width:1340px;
                               "),top=45,left=15),
             absolutePanel(img(src="black_patch.png",height="100%",width="100%",style="min-height:45px;min-width:45px;max-height:45;px;max-width:45px;
                               "),top=0,right=0),
             absolutePanel(h4(strong("# 388")),align="centre",top=75,left=32,style="color: #FDFEFE;"),
             absolutePanel(h4(strong("Hooded Plover")),align="right",top=100,left=32,style="color: #FDFEFE;"),
             absolutePanel(h4(em("Thinornis cucullatus")),align="right",style="color: #FDFEFE;font-weight: 100",top=130,left=32),
             absolutePanel(h4("EDGE Score:      4.17"),align="right",style="color: #FDFEFE;font-weight: 100",top=170,left=32),
             absolutePanel("Ⓒ Alice Domalik",align="right",style="color: #FDFEFE;font-weight: 100",top=315,left=270),
             absolutePanel(h6(strong("Evolutionary")),top=245,left=26,style="color: #FDFEFE;"),
             absolutePanel(h6(strong("Distinctiveness:")),top=265,left=18,style="color: #FDFEFE;"),
             absolutePanel(h5(strong("15.15")),top=290,left=41,style="color: #FDFEFE;"),
             absolutePanel(h6(strong("Globally")),top=245,left=153,style="color: #FDFEFE;"),
             absolutePanel(h6(strong("Endangered:")),top=265,left=143,style="color: #FDFEFE;"),
             absolutePanel(h6(strong("Vulnerable")),align="center",top=290,left=145,style="color: #FFFF00;")             
           ),
    ### New tab
    
    tabPanel(
      ("EXPLORE"),
      sidebarLayout(
        absolutePanel(
          h3("With this tab you can:"),
          br(),
          br(),
          p(strong("Search"), "for individual species using the search box"),
          p(strong("Sort"), "species by their scores, rank, or names using the small arrows on top of each column"), 
          p(strong("Filter"), "species using the boxes at the top of each column"),
          p(strong("Create"), "your EDGE checklist by clicking on each species"),
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
          top=332,left=15,width=320,height="auto",style="padding: 20px; border-bottom: 2px solid #; background: #EAECEE;"),
        absolutePanel(
          h2(em(
            "Explore the complete EDGE bird list"),align="center"
          ),
          br(),
          DT::dataTableOutput("origTable"),
          DT::dataTableOutput("origTableSelected"),
          top=332,left=338,width="auto",height="auto",style="padding: 24px; border-bottom: 2px solid #;"
      )
    ),
              absolutePanel(img(src="NOBI_04_2018.png",height="100%",width="100%",style="min-height:296px;min-width:1246px;max-height:318;px;max-width:1340px;
                               "),top=45,left=15),
             absolutePanel(img(src="black_patch.png",height="100%",width="100%",style="min-height:45px;min-width:45px;max-height:45;px;max-width:45px;
                               "),top=0,right=0),
             absolutePanel(h4(strong("# 8")),align="centre",top=75,left=32,style="color: #FDFEFE;"),
             absolutePanel(h4(strong("Northern Bald Ibis")),align="right",top=100,left=32,style="color: #FDFEFE;"),
             absolutePanel(h4(em("Geronticus eremita")),align="right",style="color: #FDFEFE;font-weight: 100",top=130,left=32),
             absolutePanel(h4("EDGE Score:      5.78"),align="right",style="color: #FDFEFE;font-weight: 100",top=170,left=32),
             absolutePanel("Ⓒ Rafa Benjumea - Ecotono Birding",align="right",style="color: #FDFEFE;font-weight: 100",top=319,left=350),
             absolutePanel(h6(strong("Evolutionary")),top=245,left=26,style="color: #FDFEFE;"),
             absolutePanel(h6(strong("Distinctiveness:")),top=265,left=18,style="color: #FDFEFE;"),
             absolutePanel(h5(strong("19.14")),top=290,left=41,style="color: #FDFEFE;"),
             absolutePanel(h6(strong("Globally")),top=245,left=153,style="color: #FDFEFE;"),
             absolutePanel(h6(strong("Endangered:")),top=265,left=143,style="color: #FDFEFE;"),
             absolutePanel(h6(strong("Critically")),align="center",top=285,left=150,style="color: #FF8C40;"),             
             absolutePanel(h6(strong("Endangered")),align="center",top=305,left=143,style="color: #FF8C40;")
             ),
    ### New tab
    
    tabPanel(
      ("DONATE"),
sidebarLayout(
               absolutePanel(
                 h3("Join the EDGE Challenge!",align="center"),
                 br(),
                 br(),
                 p("Together we can give future generations a chance enjoy the world's most extraordinary birds!"),
                 br(),
                 p("You can help to fund future EDGE fellows by donating to the EDGE of Existence Programme."), 
                 br(),
                 p("Use the buttons below to find out the sum of your",strong("top 5 EDGE birds"), "scores."),
                 br(),
                 actionButton("action3", "eBird data",style="width:239px"),
                 actionButton("action2", "Manually entered checklists",style="width:239px"),
                 br(),
                 verbatimTextOutput("EDSCORE1", placeholder = FALSE),
                 verbatimTextOutput("EDSCORE2", placeholder = FALSE),
                 br(),
                 br(),
                 p("After you find your score go", a(href="http://edgeofexistence.org/support/donation_form.php?donationType=single&causeID=0", strong("here")),"to support EDGE Fellows with a donation for the amount of your score."),
                 br(),
                 p("Learn", a(href="http://edgeofexistence.org/birds/default.php", target="_blank",("more about EDGE scores")), "for birds."),
                 top=650,left=15,width=320,height="auto",style="padding: 20px; border-bottom: 2px solid #; background: #EAECEE;"),
               absolutePanel(
                 h2(em("Saving the world's most extraordinary birds"),align="center"),
                 br(),
                 h3(a(href="http://www.zsl.org/",("ZSL's")), a(href="https://www.edgeofexistence.org/",("EDGE of Existence Programme")),"supports local conservation leaders via EDGE Fellowships.",a(href="http://edgeofexistence.org/conservation/fellows_list.php", target="_blank",("EDGE Fellows")),"work to save the world’s most unique and most threatened species, such as the",  a(href="http://edgeofexistence.org/birds/species_info.php?id=1948", target="_blank", ("Philippine Eagle")),em("(Pithecophaga jefferyi).")),
                 h3("The Philippine Eagle was never a common bird within the four islands of the Philippines where it dwells. Habitat destruction and hunting further reduced the range and numbers of this Critically Endangered bird."),
                 h3("EDGE fellow Kahlil Panopio and", a(href="http://www.haribon.org.ph/",("Haribon")), ", a Philippine NGO, identified remaining critical habitat in Mt. Mingan. They worked with local communities and governments to shift attitudes toward conservation of this emblematic bird. Thanks to their efforts, 8,227 hectares of critical habitat will soon be declared a protected area."), 
                 top=364,left=338,width="auto",height="auto",style=" border-bottom: 2px solid #;padding:24px; background: #EAECEE;")),
             absolutePanel(img(src="Kahil-Eagles3.jpg",height=315,width="auto"),top=367,left=15),
             absolutePanel(img(src="PHEA_04_2018.png",height="100%",width="100%",style="min-height:296px;min-width:1246px;max-height:318;px;max-width:1340px;
                               "),top=45,left=15),
             absolutePanel(img(src="black_patch.png",height="100%",width="100%",style="min-height:45px;min-width:45px;max-height:45;px;max-width:45px;
                               "),top=0,right=0),
             absolutePanel(h4(strong("# 15")),align="centre",top=75,left=32,style="color: #FDFEFE;"),
             absolutePanel(h4(strong("Philippine Eagle")),align="right",top=100,left=32,style="color: #FDFEFE;"),
             absolutePanel(h4(em("Pithecophaga jefferyi")),align="right",style="color: #FDFEFE;font-weight: 100",top=130,left=32),
             absolutePanel(h4("EDGE Score:      5.64"),align="right",style="color: #FDFEFE;font-weight: 100",top=170,left=32),
             absolutePanel("Ⓒ Alain Pascua",align="right",style="color: #FDFEFE;font-weight: 100",top=315,left=270),
             absolutePanel(h6(strong("Evolutionary")),top=245,left=26,style="color: #FDFEFE;"),
             absolutePanel(h6(strong("Distinctiveness:")),top=265,left=18,style="color: #FDFEFE;font-weight: 90"),
             absolutePanel(h5(strong("16.61")),top=290,left=41,style="color: #FDFEFE;"),
             absolutePanel(h6(strong("Globally")),top=245,left=153,style="color: #FDFEFE;"),
             absolutePanel(h6(strong("Endangered:")),top=265,left=143,style="color: #FDFEFE;"),
             absolutePanel(h6(strong("Critically")),align="center",top=285,left=150,style="color: #FF8C40;"),             
             absolutePanel(h6(strong("Endangered")),align="center",top=305,left=143,style="color: #FF8540;")
             )
             )
           ))

