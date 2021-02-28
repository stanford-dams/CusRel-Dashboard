library(shiny)
library(tidyverse)
library(htmltools)
library(sp)
library(sf)
library(leaflet)
library(leaflet.extras)
library(ggmap)
library(tmaptools)

# Read in data
cus_rel_data <- read_csv("Clean-CusRel-data.csv")

# Add Resolve Time, Incident + Receive Date; Create Label for Popup
cus_rel_data <- cus_rel_data %>% 
  mutate(ResolveTime = round(as.numeric(difftime(as.POSIXct(ResolvedDateTime), as.POSIXct(ReceivedDateTime), units = "days"))), 
         ReceivedDate = as.Date(ReceivedDateTime, "%m/%d/%y", tz="PST8PDT"), 
         IncidentDate = as.Date(IncidentDateTime, "%m/%d/%y", tz="PST8PDT")) %>% 
  mutate(Label = str_c("<b>Received Date:</b> ", ReceivedDate, "<br/>",
                       "<b>Incident Date:</b> ", IncidentDate, "<br/>",
                       "<b>Location:</b> ", Location, "<br/>", 
                       "<b>Destination:</b> ", Destination , "<br/>",
                       "<b>Route:</b> ", Route, " &nbsp;&nbsp ", "<b>Vehicle #:</b> ", VehNo, "<br/>", 
                       "<b>Resolve Time:</b> ", ResolveTime, " day(s)<br/>", 
                       "<b>Reason(s):</b> ", Reasons
  )) # Add a Label for Popup

# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("AC Transit customer complaints"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId="priorities", label="Priority", selected=c("Normal", "High"), choiceNames=c("Normal", "High"), choiceValues=c("Normal", "High"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("the_map", height="800px"),
      plotOutput("distPlot")
    )
  )
)

# Define server logic
server <- function(input, output){
  output$the_map <- renderLeaflet({
    # Leaflet color palette
    contact_sources <- unique(cus_rel_data$ContactSource) # get unique contact sources
    contact_source_palette <- colorFactor(palette="Set3", domain=contact_sources)
    color_list <- contact_source_palette(contact_sources)
    cus_rel_data %>%
      leaflet() %>%
        addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
        addCircleMarkers(lat = ~Latitude, lng = ~Longitude, 
                         fill = TRUE, fillColor = ~contact_source_palette(ContactSource), 
                         fillOpacity = 0.6, stroke = FALSE, 
                         color = ~contact_source_palette(ContactSource), 
                         popup = ~Label,
                         group = "circlemarkers") %>%
        addLegend("bottomright", colors = color_list, labels = contact_sources, title = "Contact Source")
  })
  filtered_data <- reactive(
    filter(cus_rel_data, Priority %in% input$priorities)
  )
  observeEvent(filtered_data(), {
    proxy <- leafletProxy("the_map", data=filtered_data()) %>%
      clearGroup("circlemarkers") %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addCircleMarkers(lat = ~Latitude, lng = ~Longitude, 
                       fill = TRUE, fillColor = ~contact_source_palette(ContactSource), 
                       fillOpacity = 0.6, stroke = FALSE, 
                       color = ~contact_source_palette(ContactSource), 
                       popup = ~Label,
                       group = "circlemarkers")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
