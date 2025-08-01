#### Constituencies app ####

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
  tags$head(tags$style('body {background-color: #111820;}')),

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
        background-color: #111820;
        color: white;
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
        padding: 20px;
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
    h3("My Map App", style = "margin: 0;")
  ),
  
  # layout
  fluidRow(
    id="maincontent",
    column(width = 3, class = "col-sidebar", uiOutput("summarytitle"), uiOutput("summarystat")),
    column(width = 9, class = "col-map", leafletOutput("sasmap",  width = "100%", height = "100%"))
  )
)

server_constituency <- function(input, output, session) {
  
  rv <- reactiveValues()
  rv$selected <- NULL
  
  # make the map
  output$sasmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -4.3, lat = 50.55, zoom = 8) %>%
      # addMarkers(lng = -3.5, lat = 50.7, popup = "Exeter") %>% 
      addPolygons(data = constituencies, color = "#58b7d4", weight = 1.5,
                  fillOpacity = 0.0,
                  opacity = 1,
                  popup = ~PCON24NM,
                  layerId = ~PCON24NM,
                  group = "base")
    # addPolygons(data = constituencies, color = "blue", weight = 1,
    #             fillColor = "blue",
    #             fillOpacity = 0.5, 
    #             popup = ~PCON24NM,
    #             layerId = ~PCON24NM)
  })
  
  # start off unclicked
  # clicked_constituency <- reactiveVal(NULL)
  
  # change to clicked if clicked
  # observeEvent(input$sasmap_shape_click, {
  #   clicked_constituency(input$sasmap_shape_click$id)
  # })
  
  # highlight clicked polygon
  observeEvent(input$sasmap_shape_click, {
    clicked_id <- input$sasmap_shape_click$id
    
    clicked_poly <- constituencies[constituencies$PCON24NM == clicked_id,]
    clicked_data <- EDM_sf[EDM_sf$PCON24NM == clicked_id,]
    clicked_mp <- constituency_data[constituency_data$PCON24NM == clicked_id,]
    
    # 
    spill_data <- clicked_data %>%
      filter(Event.Type == "spill") %>% 
      mutate(duration      = floor(difftime(End.DT, Start.DT, units = "hours")),
             Water.Company = paste(Water.Company, "Water", sep = " ")) 
    
    n_weeks <- floor(as.numeric(EDM_max_dt - EDM_min_dt, units = "weeks"))
    
    summary_data <- data.frame(
      N_spills  = nrow(spill_data),
      N_weeks   = floor(as.numeric(EDM_max_dt - EDM_min_dt, units = "weeks")),
      Day_one   = as.Date(EDM_min_dt),
      Hrs_spill = sum(spill_data$duration),
      N_sites   = length(unique(clicked_data$Asset.ID)),
      Companies = stringr::str_flatten_comma(unique(spill_data$Water.Company), last = " and "),
      MP_email  = clicked_mp$MemberEmail,
      Hrs_site  = sum(spill_data$duration)/length(unique(clicked_data$Asset.ID))
    )
    
    # Clear any previous highlight, then add new one
    leafletProxy("sasmap") %>%
      clearGroup("highlight") %>%
      addPolygons(data = clicked_poly,
                  color = "#209fc5", weight = 3,
                  fillColor = "#209fc5", fillOpacity = 0.6,
                  layerId = "highlighted",
                  group = "highlight")
    
    
    # Describe summary stats
    output$summarytitle <- renderUI({
      h4(clicked_id)
    })
    
    output$summarystat <- renderText({
      if(!is.na("hello")){
        paste0("Since ",
              as.Date(EDM_min_dt),
              " there have been ",
              summary_data$N_spills,
              " spills across ",
              summary_data$N_sites,
              " sites in ",
              clicked_id,
              ", courtesy of ",
              summary_data$Companies,
              ". <br><br>That adds up to ",
              summary_data$Hrs_spill,
              " hours of sewage outflow in the space of ",
              summary_data$N_weeks,
              " weeks. <br><br>",
              ifelse(clicked_mp$MemberEmail != "NULL", 
                     paste0("Send these stats to ",
                           clicked_mp$MemberName, 
                           ", the local MP, at: ",
                           clicked_mp$MemberEmail
                           ),
                     paste0("We do not currently have an email address for the local MP, ",
                           clicked_mp$MemberName))
              
              )
      }
    })
    
  })
  
}


shinyApp(ui_constituency, server_constituency)
