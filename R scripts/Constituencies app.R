# =============================================================================
# Title:        Constituencies app
# Description:  Shiny dashboard for SAS which displays constituencies on a map
#               and provides information about sewage spills in those constituencies.
# Author:       Margaret Bolton 
# Dependencies: shiny, leaflet, dplyr, sf
# Inputs:       aggregated static data from water company APIs
# Outputs:      Shiny app interface
# =============================================================================

project_root <- dirname(here::here())
source(here::here(project_root, "R scripts", "Accessory functions.R"))
cloropleth_palette <- colorNumeric(palette = cloro_colour, 
                                   domain = constituencies_sf$Hrs_site_week, 
                                   na.color = "#dbdbda")

ui_constituency <- fluidPage(
  # titlePanel("This is a test"),
  # titlePanel(title = div(img(src= paste("SAS_Logo_Pack_Textured",
  #                                     "SAS_Logo_Pack_Textured",
  #                                     "RGB", "Tab",
  #                                     "Practical Exclusion",
  #                                     "Mono RGB",
  #                                     "SAS-Texture-Tab-Practical-Mono-White-RGB.svg",
  #                                     sep = "/"),
  #                            height="60px"), "Constituencies map")),
  # fonts and formatting
  # colours
  tags$head(tags$style('body {color:#FFFFFF;}')),
  tags$head(tags$style('body {background-color: #2b2b2b;}')),

  # base font
  theme = bslib::bs_theme(base_font = bslib::font_google("Roboto")),

  # Header font
  tags$head(
    tags$style(HTML("
    @font-face {
      font-family: 'HVDPosterClean';
      src: url('fonts/HVD_Poster/HVD_Poster/HVD_Poster_Clean.ttf') format('truetype');
      font-weight: normal;
      font-style: normal;
    }

    h1, h2, h3, h4 {
      font-family: 'HVDPosterClean', sans-serif;
    }
  ")
    )
  ),
  
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

      #sasmap {
        height: 100%;
      }
    "))
  ),
  
  div(
    id = "header",
    tags$img(src = paste("SAS_Logo_Pack_Textured",
                         "SAS_Logo_Pack_Textured",
                         "RGB", "Tab",
                         "Practical Exclusion",
                         "Mono RGB",
                         "SAS-Texture-Tab-Practical-Mono-White-RGB.svg",
                         sep = "/"), 
             style = "height: 60px; margin-right: 10px;"),
    h3("Constituencies map", style = "margin: 0;"),
    style="border-bottom: 2px solid #FFFFFF;"
  ),
  
  # tab formatting
  tags$head(
    tags$style(HTML("
    /* Tab text colour (unselected tabs) */
    .nav-pills > li > a {
      color: #b5b5b3;
    }

    /* Tab text on hover */
    .nav-pills > li > a:hover {
      color: #ffffff;
    }
    
    /* Active tab */
    .nav-pills {
    --bs-nav-pills-link-active-bg: #2c3d52;
    --bs-nav-pills-link-active-color: #ffffff;
    }
    
  "))
  ),
  
  # layout
  fluidRow(
    id="maincontent",
    # tab column
    column(width = 4,
           navlistPanel(well = FALSE,
             id = "sidebar_tabs",
             tabPanel("Summary",
                      uiOutput("summarytitle"),
                      # textOutput("info_text"),
                      uiOutput("summarystat")
             ),
             tabPanel("Graphs",
                      uiOutput("graphtitle"),
                      uiOutput("graph"),
                      uiOutput("dateslider")
             # ),
             # tabPanel("Details",
             #          h4("Settings Tab"),
             #          sliderInput("range", "Select Range", min = 1, max = 100, value = 50)
             )
           )
    ),
    # map column
    # column(width = 4, class = "col-sidebar", uiOutput("summarytitle"), uiOutput("summarystat")),
    column(width = 8, class = "col-map", leafletOutput("sasmap",  width = "100%", height = "100%"))
  )
  

  
  # fluidRow(
  #   id="maincontent",
  #   column(width = 4, class = "col-sidebar", uiOutput("summarytitle"), uiOutput("summarystat")),
  #   column(width = 8, class = "col-map", leafletOutput("sasmap",  width = "100%", height = "100%"))
  # )
)
  
server_constituency <- function(input, output, session) {
  # load data
  constituencies_sf <- sf::st_read(here::here(project_root, "Outputs", "constituency_sf.gpkg"))
  constituencies_sf <- constituencies_sf %>%
    mutate(across(c(Hrs_spill, Hrs_site), ~ as.difftime(.x, units = "hours")))
  
  # EDM_data <- read.csv(here::here(project_root, "Outputs", ""))
  
  
  # TODO: fix dates
  summary_dates <- read.csv(here::here(project_root, "Outputs", "summary_dates.csv")) %>% 
    mutate(Main_Start.Date = as.Date(as.POSIXct(Main_Start.Date, format = "%Y-%m-%d")),
           Main_End.Date = as.Date(as.POSIXct(Main_End.Date, format = "%Y-%m-%d %H:%M:%OS")),
           Main_duration = as.difftime(Main_duration, units = "weeks"))
  
  # Can't remember what these were for but don't seem to need them
  # rv <- reactiveValues()
  # rv$selected <- NULL
  
  # make the map
  output$sasmap <- renderLeaflet({
    leaflet() %>%
      # addProviderTiles("CartoDB.Positron") %>%
      addTiles() %>%
      setView(lng = -4.3, lat = 50.55, zoom = 8) %>%
      # addPolygons(data = constituencies, color = "#58b7d4", weight = 1.5,
      addPolygons(data = constituencies_sf, color = "#1b6062", weight = 1.5,
                  fillOpacity = 0.7,
                  fillColor = ~cloropleth_palette(Hrs_site_week),
                  
                  opacity = 1,
                  popup = ~PCON24NM,
                  layerId = ~PCON24NM,
                  group = "base")
  })
  
  # highlight clicked polygon
  observeEvent(input$sasmap_shape_click, {
    clicked_id <- input$sasmap_shape_click$id
    
    clicked_poly <- constituencies_sf[constituencies_sf$PCON24NM == clicked_id,]
    
    # Clear any previous highlight, then add new one
    leafletProxy("sasmap") %>%
      clearGroup("highlight") %>%
      addPolygons(data = clicked_poly,
                  color = "#f6f6f6", 
                  opacity = 1,
                  weight = 2,
                  fillColor = "#1b6062", fillOpacity = 1,
                  layerId = "highlighted",
                  group = "highlight")
    
    # Describe summary stats
    output$summarytitle <- renderUI({
      h4(paste0("summary: ", clicked_id))
    })
    
    if(clicked_poly$N_sites == 0){
      output$summarystat <- renderText({
        "There are no reporting CSOs within this constituency."
      })
    } else if(clicked_poly$Hrs_spill == 0){
      output$summarystat <- renderText({
        paste0("There have been no recorded sewage outflows in this constituency since ",
               date_in_text(summary_dates$Main_Start.Date))
      })
    } else {
      output$summarystat <- renderUI({
        HTML(paste0("Since ",
                    date_in_text(summary_dates$Main_Start.Date),
                    " there have been ",
                    "<span style='color:#f0515a; font-size:24px; font-family:HVDPosterClean;'>",
                    clicked_poly$N_spills,
                    "</span>",
                    " spills across ",
                    clicked_poly$N_sites,
                    " sites in ",
                    clicked_id,
                    ", courtesy of ",
                    "<span style='color:#ffdc32; font-size:20px; font-family:HVDPosterClean;'>",
                    clicked_poly$Companies,
                    "</span>",
                    ". <br><br>That adds up to ",
                    "<span style='color:#f0515a; font-size:24px; font-family:HVDPosterClean;'>",
                    ifelse(clicked_poly$Hrs_spill < 1,
                           clicked_poly$Hrs_spill,
                           floor(clicked_poly$Hrs_spill)),
                    "</span>",
                    " hours of sewage outflow in the space of ",
                    ifelse(summary_dates$Main_duration < 1, 
                           "less than a week.",
                           paste0(summary_dates$Main_duration,
                                  " weeks. <br><br>")),
                    # summary_dates$Main_duration,
                    # " weeks. <br><br>",
                    ifelse(clicked_poly$MemberEmail != "NULL", 
                           paste0("Send these stats to ",
                                  clicked_poly$MemberName, 
                                  ", the local MP, at: ",
                                  "<br><span style='font-weight:bold'>",
                                  clicked_poly$MemberEmail,
                                  "</span>"
                           ),
                           paste0("We do not currently have an email address for the local MP, ",
                                  clicked_poly$MemberName))
                    
        )
        )
      })
    }
    
    # date slider
    output$dateslider <- renderUI({
      # min_val <- min(summary_dates$Main_Start.Date, na.rm = TRUE)
      # max_val <- max(summary_dates$Main_Start.Date, na.rm = TRUE)
      
      sliderInput("daterange", "Select date range", 
                  min = summary_dates$Main_Start.Date, max = summary_dates$Main_End.Date,
                  value = c(summary_dates$Main_Start.Date, summary_dates$Main_End.Date),
                  timeFormat = "%Y-%m-%d"
                  )
    })
    
    # plot from date slider
    output$graph <- renderText({ # change to renderplot when needed
      paste0("Min first date is: ", input$daterange[1])
    })
    
  })
  
}


shinyApp(ui_constituency, server_constituency)
