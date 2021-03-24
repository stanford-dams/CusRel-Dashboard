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

# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("AC Transit Customer Complaints"),
  
  sidebarLayout(
    sidebarPanel(
      h4(textOutput("filteredRowsText", inline=TRUE)),
      sliderInput(inputId = "date",
                  label = "Complaint Date",
                  min = min(cus_rel_data$ReceivedDate),
                  max = max(cus_rel_data$ReceivedDate),
                  value = c(min(cus_rel_data$ReceivedDate),max(cus_rel_data$ReceivedDate)),
                  dragRange = TRUE
      ),
      checkboxGroupButtons(inputId="priorities", label="Priority",
                           selected=c("Normal"),
                           justified=TRUE,
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
                  choices= sort(unlist(cus_rel_data %>% select(IncidentCity) %>% filter(IncidentCity != c("NULL")) %>% unique(), use.names=FALSE)),
                  selected = unlist(cus_rel_data %>% select(IncidentCity) %>% filter(IncidentCity != c("NULL")) %>% unique(), use.names=FALSE),
                  options=list('actions-box'=TRUE, 'live-search'=TRUE, 'title'='Select Cities', 'live-search-placeholder'='Search for Cities', size=5),
                  multiple=TRUE),
      pickerInput(inputId="routes", label="Route", 
                  choices= sort(unlist(cus_rel_data %>% mutate(Route = toupper(Route)) %>% select(Route) %>% filter(Route != c("NULL", "N/A")) %>% unique(), use.names=FALSE)), 
                  selected = unlist(cus_rel_data %>% mutate(Route = toupper(Route)) %>% select(Route) %>% filter(Route != c("NULL", "N/A")) %>% unique(), use.names=FALSE),
                  options=list('actions-box'=TRUE, 'live-search'=TRUE, 'title'='Select Routes', 'live-search-placeholder'='Search for Routes', size=5),
                  multiple=TRUE),
      pickerInput(inputId="reasons", label="Complaint Reason", 
                  choices=sort(unique(cus_rel_data %>% select("Reason1", "Reason2") %>% t %>% c %>% unique)),
                  selected = sort(unique(cus_rel_data %>% select("Reason1", "Reason2") %>% t %>% c %>% unique)),
                  options=list('actions-box'=TRUE, 'live-search'=TRUE, 'title'='Select Reasons', 'live-search-placeholder'='Search for Reasons', size=5), 
                  multiple=TRUE),
      pickerInput(inputId="department", label="Department", 
                  choices=sort(unique(cus_rel_data$ForAction)), 
                  selected = sort(unique(cus_rel_data$ForAction)),
                  options=list('actions-box'=TRUE, 'live-search'=TRUE, 'title'='Select Departments', 'live-search-placeholder'='Search for Departments', size=5),
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
  priority <- unique(cus_rel_data$Priority) # get unique contact sources
  priority_palette <- colorFactor(palette="Set3", domain=priority)
  #dept <- unique(cus_rel_data$ForAction) # get unique contact sources
  #dept_palette <- colorFactor(palette="Set3", domain=dept)
  #respond <- unique(cus_rel_data$RespondVia) # get unique contact sources
  #respond_palette <- colorFactor(palette="Set3", domain=respond)
  filteredRowsText <- reactiveVal(paste("Selected", nrow(cus_rel_data), "of", nrow(cus_rel_data), "complaints"))
  # Map Output
  output$the_map <- renderLeaflet({
    color_list <- priority_palette(priority)
    #color_list <- dept_palette(dept)
    #color_list <- respond_palette(respond)
    cus_rel_data %>%
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircleMarkers(lat = ~Latitude, lng = ~Longitude, 
                         fill = TRUE, fillColor = ~priority_palette(Priority), 
                         fillOpacity = 0.6, stroke = FALSE, 
                         radius = 8,
                         color = ~priority_palette(Priority), 
                         popup = ~Label,
                         group = "circlemarkers") %>%
        addLegend("bottomright", colors = color_list, labels = priority, title = "Priority")
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
  # Update Map and filteredRowsText After Options are Changed
  observeEvent(filtered_data(), {
    proxy <- leafletProxy("the_map", data=filtered_data()) %>%
      clearGroup("circlemarkers") %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(lat = ~Latitude, lng = ~Longitude, 
                       fill = TRUE, fillColor = ~priority_palette(Priority), 
                       fillOpacity = 0.6, stroke = FALSE, 
                       radius = 8,
                       color = ~priority_palette(Priority), 
                       popup = ~Label,
                       group = "circlemarkers")
    filteredRowsText(paste("Selected", nrow(filtered_data()), "of", nrow(cus_rel_data), "complaints"))
  })
  output$filteredRowsText <- renderText({
    filteredRowsText()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
