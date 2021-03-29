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
library(shinydashboard)

# Read in data
cus_rel_data <- read_csv("Clean-CusRel-data.csv")
coc_data <- st_read("./CoC_2020")
st_crs(coc_data) <- 4326
cus_rel_sf <- st_as_sf(cus_rel_data, coords = c("Longitude", "Latitude"))
st_crs(cus_rel_sf) <- 4326
# Initial CoC Cus Rel Data
cus_rel_coc <- st_intersection(coc_data, cus_rel_sf)
cus_rel_coc <- cus_rel_coc %>%
  group_by(geoid) %>% 
  summarize(n = n()) %>% 
  st_drop_geometry() %>% 
  left_join(coc_data, .) %>% 
  mutate(n = replace_na(n, 0),
         Opacity = n/ifelse(max(n) == 0, 1, max(n)), 
         Popup = str_c("<b>Tract:</b> ", tract, "&nbsp;&nbsp", "<b>Population:</b> ", tot_pop,  "<br/>", 
                       "<b># of Complaints: </b>", n, "<br/>", 
                       "<b>CoC Class:</b> ", coc_class)
  )

# Define UI
ui <- dashboardPage(
  
  skin = "green",
  # Application title
  dashboardHeader(title = "AC Transit Customer Complaints", 
                  titleWidth = 350),
  
  dashboardSidebar(
      h4(textOutput("filteredRowsText", inline = TRUE)),
      column(width = 12, align = "center", actionButton("refreshCoC", "Refresh communities of concern map")),
      width = 350, 
      column(width = 12, align = "center", sliderInput(inputId = "date",
                  label = "Complaint Date",
                  min = min(cus_rel_data$ReceivedDate),
                  max = max(cus_rel_data$ReceivedDate),
                  value = c(min(cus_rel_data$ReceivedDate),max(cus_rel_data$ReceivedDate)),
                  dragRange = TRUE
      )),
      checkboxGroupButtons(inputId = "priorities", label = "Priority", justified = TRUE, 
                           selected = c("Normal", "High"),
                           choices = c("Normal", "High")),
      checkboxGroupButtons(inputId = "respondVia", label = "Respond Via", justified = TRUE, 
                         selected = c("App", "Email", "Letter", "Phone", "None"), 
                         choices = c("App", "Email", "Letter", "Phone", "None")),
      checkboxGroupInput(inputId = "contact", label = "Contact Source", inline = TRUE, 
                         selected = c("WEB", "Phone", "SocialMedia", "Email", "Operations", "BoardofDirectors","Letter","App","WalkIn","Five11"), 
                         choiceNames = c("WEB", "Phone", "Social Media", "Email", "Operations", "Board of Directors","Letter","App","Walk-In","511"), 
                         choiceValues = c("WEB", "Phone", "SocialMedia", "Email", "Operations", "BoardofDirectors","Letter","App","WalkIn","Five11")),
      pickerInput(inputId = "cities", label = "Incident City", width = "250px", 
                  choices = sort(unlist(cus_rel_data %>% select(IncidentCity) %>% unique(), use.names = FALSE)),
                  selected = unlist(cus_rel_data %>% select(IncidentCity) %>% unique(), use.names = FALSE),
                  options = list('actions-box' = TRUE, 'live-search' = TRUE, 'title' = 'Select Cities', 'live-search-placeholder' = 'Search for Cities', 'selected-text-format' = 'count > 3', 'size' = 5),
                  multiple = TRUE),
      pickerInput(inputId = "routes", label = "Route", width = "250px", 
                  choices = cus_rel_data %>% pull(Route) %>% unique(), 
                  selected = cus_rel_data %>% pull(Route) %>% unique(),
                  options = list('actions-box' = TRUE, 'live-search' = TRUE, 'title' = 'Select Routes', 'live-search-placeholder' = 'Search for Routes', 'selected-text-format' = 'count > 3', 'size' = 5),
                  multiple = TRUE),
      pickerInput(inputId = "reasons", label = "Complaint Reason", width = "250px", 
                  choices = sort(cus_rel_data %>% select("Reason1", "Reason2") %>% t %>% c %>% unique),
                  selected = sort(cus_rel_data %>% select("Reason1", "Reason2") %>% t %>% c %>% unique),
                  options = list('actions-box' = TRUE, 'live-search' = TRUE, 'title' = 'Select Reasons', 'live-search-placeholder' = 'Search for Reasons', 'selected-text-format' = 'count > 3', 'size' = 5), 
                  multiple = TRUE),
      pickerInput(inputId = "department", label = "Department", width = "250px", 
                  choices = sort(unique(cus_rel_data$ForAction)), 
                  selected = sort(unique(cus_rel_data$ForAction)),
                  options = list('actions-box' = TRUE, 'live-search' = TRUE, 'title' = 'Select Departments', 'live-search-placeholder' = 'Search for Departments', 'selected-text-format' = 'count > 3', 'size' = 5),
                  multiple = TRUE)
    ),
  
    # Show a plot of the generated distribution
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ), 
      fluidRow(
        tabBox(id = "tabset1", width = 12, 
          tabPanel("Stops Map", leafletOutput("point_map", height = "650px")), 
          tabPanel("Communities of Concern", leafletOutput("CoC_map", height = "650px")), 
          tabPanel("Raw Data", 
                   numericInput("maxrows", "Rows to show", 25), 
                   verbatimTextOutput("dataTable"), 
                   downloadButton("downloadCsv", "Download as CSV"))
        )
      )
    )
)

# Define server logic
server <- function(input, output){
  # Leaflet color palettes
  colors <- c("#00a65a", "#dd4b39")
  levels <- c("Normal", "High")
  priority_palette <- colorFactor(colors, levels = levels)
  coc_palette <- colorNumeric(c("white", "#db1a02"), domain = c(0, 1))
  # Heatmap hover labels
  Labels <- str_c("<b>Complaints:</b> ", cus_rel_coc$n) %>% lapply(htmltools::HTML)
  # Filtered rows text
  filteredRowsText <- reactiveVal(paste("Selected", nrow(cus_rel_data), "of", nrow(cus_rel_data), "complaints"))
  # Map Output
  output$point_map <- renderLeaflet({
    cus_rel_data %>%
      leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(lat = ~Latitude, lng = ~Longitude, 
                       fill = TRUE, fillColor = ~priority_palette(Priority), 
                       fillOpacity = 0.6, stroke = TRUE, 
                       radius = 8, 
                       color = "#000", weight = 4, opacity = 0.1, 
                       popup = ~Label,
                       group = "circlemarkers") %>%
      addLegend("bottomright", colors = colors, labels = levels, title = "Priority")
  })
  # CoC Heat Map
  output$CoC_map <- renderLeaflet({
    cus_rel_coc %>% 
      leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(color = "#c7c7c7", weight = 0.3, opacity = 1, 
                  fill = TRUE, fillColor = "#dd4b39", fillOpacity = ~Opacity, 
                  highlight = highlightOptions(
                    weight = 2, color = "#7d7d7d", 
                    bringToFront = TRUE), 
                  label = Labels, 
                  popup = ~Popup, 
                  group = "coc_polygons") %>% 
      addLegend("bottomright", pal = coc_palette, 
                values = c(0, 1), bins = c(0.00, 0.25, 0.50, 0.75, 1.00), 
                title = "Relative<br/>Complaint<br/>Density")
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
    proxy <- leafletProxy("point_map", data = filtered_data()) %>%
      clearGroup("circlemarkers") %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(lat = ~Latitude, lng = ~Longitude, 
                       fill = TRUE, fillColor = ~priority_palette(Priority), 
                       fillOpacity = 0.6, stroke = TRUE, 
                       radius = 8, 
                       color = "#000", weight = 4, opacity = 0.1, 
                       popup = ~Label,
                       group = "circlemarkers")
      filteredRowsText(paste("Selected", nrow(filtered_data()), "of", nrow(cus_rel_data), "complaints"))
  }) 
  observeEvent(input$refreshCoC, {
    # Create new CoC Data
    cus_rel_sf <- st_as_sf(filtered_data(), coords = c("Longitude", "Latitude"))
    st_crs(cus_rel_sf) <- 4326
    cus_rel_coc <- st_intersection(coc_data, cus_rel_sf)
    cus_rel_coc <- cus_rel_coc %>%
      group_by(geoid) %>% 
      summarize(n = n()) %>% 
      st_drop_geometry() %>% 
      left_join(coc_data, .) %>% 
      mutate(n = replace_na(n, 0),
             Opacity = n/ifelse(max(n) == 0, 1, max(n)), 
             Popup = str_c("<b>Tract:</b> ", tract, "&nbsp;&nbsp", "<b>Population:</b> ", tot_pop,  "<br/>", 
                           "<b># of Complaints: </b>", n, "<br/>", 
                           "<b>CoC Class:</b> ", coc_class)
      )
    Labels <- str_c("<b>Complaints:</b> ", cus_rel_coc$n) %>% lapply(htmltools::HTML)
    proxy_coc <- leafletProxy("CoC_map", data = cus_rel_coc) %>% 
      clearGroup("coc_polygons") %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(color = "#c7c7c7", weight = 0.3, opacity = 1, 
                  fill = TRUE, fillColor = "#dd4b39", fillOpacity = ~Opacity, 
                  highlight = highlightOptions(
                    weight = 2, color = "#7d7d7d", 
                    bringToFront  =  TRUE), 
                  label = Labels, 
                  popup = ~Popup, 
                  group = "coc_polygons")
  })
  # Number of Complaints Shown
  output$filteredRowsText <- renderText({
    filteredRowsText()
  })
  # Download CSV of filtered complaints
  output$downloadCsv <- downloadHandler(
    filename = "Bus_Complaints.csv",
    content = function(file) {
      write.csv(filtered_data()[,names(filtered_data()) != "Label"], file)
    },
    contentType = "text/csv"
  )
  # Display filtered data
  output$dataTable <- renderPrint({
    orig <- options(width = 1000)
    print(tail(filtered_data()[,names(filtered_data()) != "Label"], input$maxrows), row.names = FALSE)
    options(orig)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
