# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dplyr)
library(myebird)
library(DT)
library(shinythemes)

ED <- as.data.frame(edge)

function(input, output) {
  # save clean data as reactive object that can be used further down
  mydata <- reactive({
    if(!is.null(input$mydata)) {
      mydata <- ebirdclean(input$mydata$datapath)
      return(mydata)
    }
    else {
      return(data.frame(NULL))
    }
  })
  edgedat <- reactive({
    if(!is.null(input$mydata)) {
      if (input$grouping == TRUE) {
        edgedat <- group_by(mydata(), Country) %>%
          do(myedge(.))
      } else {
        edgedat <- myedge(mydata())
      }
    }
    else {
      return(data.frame(NULL))
    }
  })
  output$phylo.results <- renderTable({
    if(!is.null(input$mydata)) {
      out <- edgedat() %>% select(-sciName.edge) %>%
        dplyr::filter(EDGE.Rank <= input$edgerankfilter) %>%
        dplyr::filter(ED.Score > input$edfilter[1] &
                        ED.Score < input$edfilter[2])
      if (input$sort_ed) {
        if (input$grouping) {
          out <- dplyr::arrange(out, Country, -ED.Score)
        } else
          out <- dplyr::arrange(out, -ED.Score)
      }
      out <- rename(out, `Common name` = comName,
                    `Scientific name` = sciName,
                    `ED Score` = ED.Score,
                    `EDGE Score` = EDGE.Score,
                    `EDGE Rank` = EDGE.Rank)
    }
  })
    ####Calculates top5 from ebird data
  
  Edscore2 <- eventReactive(input$action3, {
    ## to attempt to estumate top5 for each country
    # if (input$grouping) {
    #   y1 <- group_by(edgedat(), Country)
    # } else {
    #   y1 <- edgedat()
    # }
    y1 <- edgedat() %>% ungroup
    
    y2 <- arrange(y1, desc(EDGE.Score))
    y3 <- y2 %>% top_n(n = 5, wt = EDGE.Score) 
    y4 <- y3 %>% summarise(sum = sum(EDGE.Score) %>% 
                             round(digits = 2))
    y5 <- as.character(y4[1])
    })
  
  output$EDSCORE2 <- Edscore2
  
  
  
  
  # PD is calculated in this renderTable
  output$summary.tab <- renderTable({
    if(!is.null(input$mydata)) {
      if(input$pd == TRUE) {
        if (input$grouping) {
          pddata <- mydata() %>% 
            group_by(Country) %>%
            do(mypd(., ntrees = 5)) %>%
            select(Country, median_pd) %>%
            rename(`Median PD` = median_pd)
        } else {
          pddata <- mypd(mydata(), ntrees = 5) %>%
            select(median_pd) %>%
            rename(`Median PD` = median_pd)
        }
      }
      if (input$grouping) {
        sumout <- edgedat() %>% group_by(Country) %>%
          summarise(`Sum of ED Scores` = sum(ED.Score), 
                    `Sum of EDGE Scores` = sum(EDGE.Score)) 
      } else {
        sumout <- summarise(edgedat(), `Sum of ED Scores` = sum(ED.Score), 
                            `Sum of EDGE Scores` = sum(EDGE.Score))
      }
      
      if (input$pd) {
        if(input$grouping) {
          return(left_join(pddata, sumout, by = "Country"))
        } else {
          return(cbind(pddata, sumout))
        }
      } else
        return(sumout)
    }
  })

  ###Create selectable Original ED file from my ebird 
  output$origTable <- DT::renderDataTable({
    datatable(
    ED, options = list(pageLength=10,order = list(list(4, "desc")), 
                      columnDefs = list(list(visible = FALSE, targets = c(0,6)))),
    selection = list(mode = "multiple"),
    filter=list(position="top",clear=FALSE,plain=TRUE), 
    colnames=c("","Scientific Name","Common Name","ED Score","EDGE Score","EDGE Rank","Hidden key for outdated names")
    ) %>% formatRound(c("ED.Score","EDGE.Score"),digits=3)
  })
  
  origTable_selected <- reactive(as.data.frame({
    ids <- input$origTable_rows_selected
    ED[ids,]
  }))
  
  ###Render a new table with selected species
  output$origTableSelected <- DT::renderDataTable({
    datatable(
      origTable_selected(),colnames=c("My Ranking","Scientific Name","Common Name","ED Score","EDGE Score","EDGE Rank","Hidden key for outdated names"),
      options = list(dom = "t",order = list(list(4, "des")),
                   scrollY = '250px', paging = FALSE ,
                   columnDefs = list(list(visible = FALSE, targets = c(6)))),
      selection = list(mode = "multiple"),
      caption = "My EDGE checklist",
      callback=JS("table.on( 'order.dt search.dt', function () {
                                table.column(0, {search:'applied', order:'applied'}).nodes().each( function (cell, i) {
                                                cell.innerHTML = i+1;});}).draw();")
      ) %>% formatRound(c("ED.Score","EDGE.Score"),digits=3)
  
  })
  
  ##Save the new table as csv.
  output$downloadData <- downloadHandler('myedge.csv', content = function(file) {
    s <- input$origTable_rows_selected
    write.csv(ED[s, , drop = FALSE], file)
  })
  
  ###Obtain PD2###
  pd2<-eventReactive(input$action,{
    pd2b<-mypd(origTable_selected(), ntrees = 5)
  pd2c<-as.character(round(pd2b[2],digits=2))
  })
  output$pd2<-pd2
  
  ###Obtain sum of TOP 5 birds
  Edscore1 <- eventReactive(input$action2, {
      x2 <- head(arrange(origTable_selected()),desc(EDGE.Score), n = 5)
      x3<-x2 %>% 
        summarise(sum(EDGE.Score),n = n())
      x4<-round(x3,digits=2)
      x5<-as.character(x4[1])})
      
  output$EDSCORE1<-Edscore1
    
}
