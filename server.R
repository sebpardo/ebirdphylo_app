# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dplyr)
library(myebird)

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
}
