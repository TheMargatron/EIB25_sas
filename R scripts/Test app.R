# =============================================================================
# Title:        Test app
# Description:  Mini self-contained shiny app for testing out featurse on the website
# Author:       Margaret Bolton 
# Dependencies: shiny, leaflet, dplyr, sf
# Inputs:       None
# Outputs:      Shiny app interface
# =============================================================================

library(shiny)
library(ggplot2)

ui_test <- fluidPage(
  # content
  div(
    id = "header",
    h3("Constituencies map", style = "margin: 0;"),
    style="border-bottom: 2px solid #FFFFFF;"
  ), # end header
  
  fluidRow(
    id = "maincontent",
    column(width = 4,
           id = "sidebar",
           navlistPanel(
             well = FALSE,
             id = "sidebar_tabs",
             tabPanel("Summary",
                      textOutput("summarytext")),
             tabPanel("Graph",
                      plotOutput("summaryplot")),
             tabPanel("Details",
                      textOutput("detailstext"))
           ) # end navlistpanel
    ), # end column1
    column(width = 8,
           class = "col-map",
           div(
             "Map",
             id = "map_placeholder",
             style = "
          border: 2px dashed #999;
          display: flex;
          align-items: center;
          justify-content: center;
          font-size: 24px;
          color: #666;
        "
           ))
  ), # end fluidrow
  
  # layout
  tags$head(
    tags$style(HTML("
      html, body {
        height: 100vh;
        margin: 0;
        padding: 0;
      }
      
      #header {
        height: 60px;
        display: flex;
        align-items: center;
        padding-left: 10px;
      }
      
      #maincontent {
        height: calc(100vh - 60px);
        margin: 0;
      }
      .col-sidebar {
        height: 100%;
        overflow-y: auto;
        padding-top: 60px;
        padding-left: 50px;
        padding-right: 50px;
      }
      .col-map {
        height: 100%;
        padding: 0;
      }
      #map_placeholder {
        height: 100%;
      }
      
    "))
  )
  
) # end fluidpage

server_test <- function(input, output, session) {
  output$summarytext <- renderText(
    "Hello world! This will be some text generated and formatted for each constituency"
  )
  
  output$summaryplot <- renderPlot({
    fake_data <- data.frame(x_var = runif(20),
                            y_var = rnorm(20))
    ggplot(fake_data, aes(x = x_var, y = y_var)) +
      geom_point()
  })
  
  output$detailstext <- renderText({
    "Some text describing how data is sourced and processed, and a legend"
  })
  
} # end server

shinyApp(ui_test, server_test)
