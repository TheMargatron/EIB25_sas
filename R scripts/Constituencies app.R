#### Constituencies app ####

ui_constituency <- fluidPage(
  # titlePanel("SAS data vis map"),
  fluidRow(
    column(width = 3, uiOutput("summarytitle"), tableOutput("summarystat")),
    column(width = 7, leafletOutput("sasmap", height = "600px"))
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
      addMarkers(lng = -3.5, lat = 50.7, popup = "Exeter") %>% 
      addPolygons(data = constituencies, color = "#fcd518", weight = 1,
                  fillOpacity = 0.4,
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
    
    clicked_poly <- constituencies[constituencies$PCON24NM == clicked_id, ]
    clicked_data <- EDM_sf[EDM_sf$PCON24NM == clicked_id,]
    
    # 
    spill_data <- clicked_data %>%
      filter(Event.Type == "spill") %>% 
      mutate(duration = End.DT - Start.DT,
             Water.Company = paste(Water.Company, "Water", sep = " ")) 
    
    n_weeks <- floor(as.numeric(EDM_max_dt - EDM_min_dt, units = "weeks"))
    
    summary_data <- data.frame(
      N_events = paste(nrow(spill_data),
                       "spills since",
                       as.Date(EDM_min_dt),
                       # "spills over", 
                       # as.character(n_weeks), 
                       # "weeks", 
                       sep = " "),
      Hrs_spill = sum(spill_data$duration),
      N_sites = length(unique(clicked_data$Asset.ID))
    ) %>% 
      mutate(
        # Hrs_per_site = as.numeric(Hrs_spill/N_sites, units = "hours"),
        Hrs_per_site = paste(floor(as.numeric(Hrs_spill/N_sites, units = "hours")),
                             "hours per site",
                             sep = " "),
        N_sites = paste(length(unique(clicked_data$Asset.ID)),
                        "sites",
                        sep = " "),
        Hrs_spill = paste(floor(as.numeric(Hrs_spill, units = "hours")),
                          "hours over",
                          as.character(n_weeks),
                          "weeks",
                          sep = " "),
        Water_company = paste("Courtesy of",
          stringr::str_flatten_comma(unique(spill_data$Water.Company), last = " and ")),
        MPs_email = paste("Send this to",
                          "YourLocalMP@email.com",
                          sep = " ")
      )
    
    
    # Clear any previous highlight, then add new one
    leafletProxy("sasmap") %>%
      clearGroup("highlight") %>%
      addPolygons(data = clicked_poly,
                  color = "#57c2e2", weight = 3,
                  fillColor = "#57c2e2", fillOpacity = 0.6,
                  layerId = "highlighted",
                  group = "highlight")
    
    
    # Produce table of summary stats
    output$summarytitle <- renderUI({
      h4(clicked_id)
    })

    output$summarystat <- renderTable({
      # Replace with the actual columns you want to show
      data.frame(
        # Statistic = names(summary_data),
        Value = as.character(t(summary_data)[,1]),
        row.names = NULL
      )
    }, colnames = FALSE)
    
    
  })
  
}


shinyApp(ui_constituency, server_constituency)
