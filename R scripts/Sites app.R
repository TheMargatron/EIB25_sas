#### Sites app ####

# Chose Positron for now, but ideally would use one which has blue sea
# E.g. MapTiler Basic, but that requires API and limited usage

ui <- fluidPage(
  # titlePanel("SAS data vis map"),
  fluidRow(
    column(width = 3, plotOutput("barplot", height = "300px")),
    column(width = 7, leafletOutput("sasmap", height = "600px"))
  )
)

server <- function(input, output, session) {
  
  # make the map
  output$sasmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -3.5, lat = 50.7, zoom = 8) %>%
      addMarkers(lng = -3.5, lat = 50.7, popup = "Exeter") %>% 
      addCircleMarkers(data = CSO_data, lng = ~Longitude, lat = ~Latitude, 
                       radius = 3, weight = 1,
                       fillColor = "#57c2e2", color = "black", fill = TRUE,
                       layerId = ~Asset.ID,
                       group = "points")
  })
  
  # start off unclicked
  clicked_site <- reactiveVal(NULL)
  
  # change to clicked if clicked
  observeEvent(input$sasmap_marker_click, {
    clicked_site(input$sasmap_marker_click$id)
    
    clicked_point <- EDM_data[EDM_data$Asset.ID == clicked_site(), ]
    river_points <- EDM_data[EDM_data$Receiving.Water %in% clicked_point$Receiving.Water, ]
    
    leafletProxy("sasmap") %>%
      clearGroup("highlight") %>%
      clearGroup("river") %>%
      addCircleMarkers(
        data = river_points,
        lng = ~lng, lat = ~lat,
        radius = 5,
        color = "#fcd518",
        fillColor = "#fcd518",
        fillOpacity = 0.5,
        group = "river"
      ) %>% 
      addCircleMarkers(
        data = clicked_point,
        lng = ~lng, lat = ~lat,
        radius = 5,
        color = "#d3af00",
        fillColor = "#d3af00",
        fillOpacity = 1,
        group = "highlight"
      )
    
  })
  
  # highlight the point 
  
  
  # highlight points on the same river
  
  
  # render the barplot
  output$barplot <- renderPlot({
    req(clicked_site())  # Only run if a marker was clicked
    
    SWW_spills <- EDM_full_status %>% 
      filter(Asset.ID == clicked_site()) %>%
      filter(Event.Type == "spill")
    
    SWW_filtered <- EDM_full_status %>% 
      filter(Asset.ID == clicked_site()) %>%
      mutate(Status = factor(Event.Type, 
                             levels = c("spill", "maintenance", "none"),
                             labels = c("Spill", "Offline", "None"),
                             ordered = TRUE))
    
    EDM_min_dt <- min(EDM_data$Start.DT, na.rm = TRUE)
    EDM_max_dt <- max(EDM_data$End.DT, na.rm = TRUE)
    EDM_med_dt <- median(EDM_min_dt, EDM_max_dt)
    
    SWW_dates <- data.frame(DT = c(EDM_min_dt,
                                   EDM_max_dt,
                                   EDM_med_dt),
                            stat = c("min", "max", "med")) %>% 
      mutate(label_date = as.Date(DT))
    
    ggplot() +
      geom_point(data = SWW_spills, aes(y = 0.5, x = Start.DT), size = 3, shape = 6, color = "#cf121d") +
      geom_segment(data = SWW_filtered, aes(y = 0, x = Start.DT, xend = End.DT, color = Status), linewidth = 6, show.legend = TRUE) +
      geom_text(data = SWW_dates, aes(y = -0.5, x = DT, label = label_date)) +
      ylim(c(-1,1)) +
      labs(x = "Time", y = "Category", title = unique(SWW_filtered$Asset.ID)) +
      theme_void() +
      theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            legend.position = "top",
            aspect.ratio = 0.3) +
      theme(plot.title = element_text(vjust=5)) +
      scale_colour_manual(
        values = c("#cf121d", "#585c62", "#1d9b7e"),
        drop = FALSE
      )
    
  })
  
}


shinyApp(ui, server)

