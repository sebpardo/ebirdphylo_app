
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dplyr)
library(myebird)

function(input, output) {
  output$phylo.results <- renderTable({
  if(!is.null(input$mydata)) {
    mydata <- ebirdclean(input$mydata$datapath)
    if (input$grouping == TRUE) {
      edgedat <- group_by(mydata, Country) %>%
        do(myedge(., edge.cutoff = input$edgerankfilter))
    } else {
      edgedat <- myedge(mydata, edge.cutoff = input$edgerankfilter)
    }
  out <- edgedat %>% select(-sciName.edge) %>%
      dplyr::filter(ED.Score > input$edfilter[1] &
                    ED.Score < input$edfilter[2] #&
                   # EDGE.Score > input$edgefilter[1] &
                   #EDGE.Score < input$edgefilter[2]
                   )
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

    return(out)
  } 
    else {
    # if (input$allbirds) {
    #   out <- myebird::edge %>% select(-sciName.edge) %>%
    #     dplyr::filter(ED.Score > input$edfilter[1] &
    #                     ED.Score < input$edfilter[2] &
    #                     EDGE.Score > input$edgefilter[1] &
    #                     EDGE.Score < input$edgefilter[2]) %>%
    #     rename(`Common name` = comName,
    #            `Scientific name` = sciName,
    #            `ED Score` = ED.Score,
    #            `EDGE Score` = EDGE.Score,
    #            `EDGE Rank` = EDGE.Rank)
    #   return(out)
    # } 
    #  else {
        return(data.frame(NULL))
    #  }
    }
  })
}
