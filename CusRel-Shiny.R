library(shiny)
library(shinyWidgets)
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
                       "<b>Reason(s):</b> ", Reason1, if_else(is.na(Reason2), "", str_c("; ", Reason2))
                       )) # Add a Label for Popup

# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("AC Transit Customer Complaints"),
  
  sidebarLayout(
    sidebarPanel(
      
      sliderInput(inputId = "date",
                  label = "Complaint Date",
                  min = min(cus_rel_data$ReceivedDate),
                  max = max(cus_rel_data$ReceivedDate),
                  value = c(min(cus_rel_data$ReceivedDate),max(cus_rel_data$ReceivedDate)),
                  dragRange = TRUE
      ),
      checkboxGroupInput(inputId="priorities", label="Priority", 
                         selected=c("Normal", "High"), 
                         choiceNames=c("Normal", "High"), 
                         choiceValues=c("Normal", "High")),
      checkboxGroupInput(inputId="respondVia", label="Respond Via", 
                         selected=c("App", "Email", "Letter", "Phone", "None"), 
                         choiceNames=c("App", "Email", "Letter", "Phone", "None"), 
                         choiceValues=c("App", "Email", "Letter", "Phone", "None")),
      checkboxGroupInput(inputId="contact", label="Contact Source", 
                         selected=c("WEB", "Phone", "SocialMedia", "Email", "Operations", "BoardofDirectors","Letter","App","WalkIn","Five11"), 
                         choiceNames=c("WEB", "Phone", "Social Media", "Email", "Operations", "Board of Directors","Letter","App","Walk-In","511"), 
                         choiceValues=c("WEB", "Phone", "SocialMedia", "Email", "Operations", "BoardofDirectors","Letter","App","WalkIn","Five11")),
      pickerInput(inputId="cities", label="Incident City",
                  choices=sort(unique(cus_rel_data$IncidentCity)),
                  selected = (unique(cus_rel_data$IncidentCity)),
                  options=list('actions-box'=TRUE, 'live-search'=TRUE, size=5),
                  multiple=TRUE),
      pickerInput(inputId="routes", label="Route", 
                  choices=sort(unique(cus_rel_data$Route)), 
                  selected = (unique(cus_rel_data$Route)),
                  options=list('actions-box'=TRUE, 'live-search'=TRUE, size=5), 
                  multiple=TRUE),
      pickerInput(inputId="reasons", label="Complaint Reason", 
                  choices=sort(unique(cus_rel_data %>% select("Reason1", "Reason2") %>% t %>% c %>% unique)),
                  selected = (unique(cus_rel_data %>% select("Reason1", "Reason2") %>% t %>% c %>% unique)),
                  options=list('actions-box'=TRUE, 'live-search'=TRUE, size=5), 
                  multiple=TRUE),
      pickerInput(inputId="department", label="Department", 
                  choices=sort(unique(cus_rel_data$ForAction)), 
                  selected = unique(cus_rel_data$ForAction),
                  options=list('actions-box'=TRUE, 'live-search'=TRUE, size=5),
                  multiple=TRUE)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("the_map", height="700px"),
      plotOutput("distPlot")
    )
  )
)

# Define server logic
server <- function(input, output){
  # Leaflet color palette
  contact_sources <- unique(cus_rel_data$ContactSource) # get unique contact sources
  contact_source_palette <- colorFactor(palette="Set3", domain=contact_sources)
  #dept <- unique(cus_rel_data$ForAction) # get unique contact sources
  #dept_palette <- colorFactor(palette="Set3", domain=dept)
  #respond <- unique(cus_rel_data$RespondVia) # get unique contact sources
  #respond_palette <- colorFactor(palette="Set3", domain=respond)
  
  # Map Output
  output$the_map <- renderLeaflet({
    color_list <- contact_source_palette(contact_sources)
    #color_list <- dept_palette(dept)
    #color_list <- respond_palette(respond)
    cus_rel_data %>%
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircleMarkers(lat = ~Latitude, lng = ~Longitude, 
                         fill = TRUE, fillColor = ~contact_source_palette(ContactSource), 
                         fillOpacity = 0.6, stroke = FALSE, 
                         radius = 8,
                         color = ~contact_source_palette(ContactSource), 
                         popup = ~Label,
                         group = "circlemarkers") %>%
        addLegend("bottomright", colors = color_list, labels = contact_sources, title = "Contact Source")
  })
  # Filter Data Reactively without Shifting Map
  filtered_data <- reactive(
    filter(cus_rel_data, 
           Priority %in% input$priorities,
           IncidentCity %in% input$cities, 
           Route %in% input$routes,
           RespondVia %in% input$respondVia, 
           ForAction %in% input$department, 
           Reason1 %in% input$reasons | Reason2 %in% input$reasons,
           between(ReceivedDate, input$date[1], input$date[2]),
           ContactSource %in% input$contact)
  )
  # Update Map After Options are Changed
  observeEvent(filtered_data(), {
    proxy <- leafletProxy("the_map", data=filtered_data()) %>%
      clearGroup("circlemarkers") %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(lat = ~Latitude, lng = ~Longitude, 
                       fill = TRUE, fillColor = ~contact_source_palette(ContactSource), 
                       fillOpacity = 0.6, stroke = FALSE, 
                       radius = 8,
                       color = ~contact_source_palette(ContactSource), 
                       popup = ~Label,
                       group = "circlemarkers")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
