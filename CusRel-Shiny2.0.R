library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(tidyverse)
library(htmltools)
library(sp)
library(sf)
library(leaflet)
library(leaflet.extras)
library(ggmap)
library(tmaptools)
library(shinydashboard)
library(DT)
library(lubridate)
library(grid)
library(gridExtra)

select <- dplyr::select

# Read in cusrel data, getting rid of time zone attribute
cus_rel_data <- read_csv("Clean-CusRel-data.csv", na="") %>%
  mutate(ReceivedDateTime=as.character(ReceivedDateTime),
         ResolvedDateTime=as.character(ResolvedDateTime),
         IncidentDateTime=as.character(IncidentDateTime),
         updatedOn=as.character(updatedOn))
  
# Convert cusrel data to sf
cus_rel_sf <- st_as_sf(cus_rel_data, coords = c("Longitude", "Latitude"),
                       remove=FALSE)
st_crs(cus_rel_sf) <- 4326

# Get bbox of entire dataset
min_lon <- st_bbox(cus_rel_sf)$xmin-0.01
max_lon <- st_bbox(cus_rel_sf)$xmax+0.01
min_lat <- st_bbox(cus_rel_sf)$ymin-0.01
max_lat <- st_bbox(cus_rel_sf)$ymax+0.01
bbox <- st_sfc(st_polygon(list(matrix(c(min_lon, min_lat, 
                                 max_lon, min_lat, 
                                 max_lon, max_lat, 
                                 min_lon, max_lat, 
                                 min_lon, min_lat), 
                                 ncol=2, byrow=TRUE))), crs=4326)

# Read in CoC data, convert to SF
coc_data <- st_read("./CoC_2020")
st_crs(coc_data) <- 4326

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
                       "<b>CoC Class:</b> ", coc_class, "<br/>", 
                       "<b>% Low Income:</b> ", round(pct_below2*10, 2), "<br/>", 
                       "<b>% Minority:</b> ", round(pct_minori*10, 2), "<br/>", 
                       "<b>% Disabled:</b> ", round(pct_disab*10, 2), "<br/>", 
                       "<b>% Over 75:</b> ", round(pct_over75*10, 2), "<br/>", 
                       "<b>% 0-Vehicle Household:</b> ", round(pct_zvhhs*10, 2), "<br/>", 
                       "<b>% Single Parent Family:</b> ", round(pct_spfam*10, 2))
  )

# Define UI
ui <- dashboardPage(
  
  skin = "green", 
  # Application title
  dashboardHeader(title = "AC Transit Customer Complaints", 
                  titleWidth = 380), 
  
  dashboardSidebar(
      fluidRow(column(width = 12, align = "center", style="padding-top: 12px;", 
                      h4(textOutput("filteredRowsText", inline = TRUE)))),
      width = 380,
      # Old Time Input
      #column(width = 12, align = "center", sliderInput(inputId = "date",
                  #label = "Complaint Date",
                  #min = min(cus_rel_data$ReceivedDate),
                  #max = max(cus_rel_data$ReceivedDate),
                  #value = c(min(cus_rel_data$ReceivedDate),max(cus_rel_data$ReceivedDate)),
                  #dragRange = TRUE
      #)),
      fluidRow(column(width = 12, align = "center",
             
             dateRangeInput(inputId = "date", 
                label = "Complaint Date",
                start = min(cus_rel_data$ReceivedDate),
                end = max(cus_rel_data$ReceivedDate),
                min = min(cus_rel_data$ReceivedDate),
                max = max(cus_rel_data$ReceivedDate)),
             
             checkboxGroupButtons(inputId = "priorities", label = "Priority", justified = TRUE, 
                           selected = c("Normal", "High"),
                           choices = c("Normal", "High")),
             checkboxGroupButtons(inputId = "receiveddateday", label = "Day of the Week", justified = TRUE, 
                                  selected = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
                                  choiceNames = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"), 
                                  choiceValues = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
             checkboxGroupButtons(inputId = "respondVia", label = "Respond Via", justified = TRUE, 
                         selected = c("App", "Email", "Letter", "Phone", "None"), 
                         choices = c("App", "Email", "Letter", "Phone", "None")),
             checkboxGroupInput(inputId = "contact", label = "Contact Source", inline = TRUE, 
                         selected = c("WEB", "Phone", "SocialMedia", "Email", "Operations", "BoardofDirectors","Letter","App","WalkIn","Five11"), 
                         choiceNames = c("WEB", "Phone", "Social Media", "Email", "Operations", "Board of Directors","Letter","App","Walk-In","511"), 
                         choiceValues = c("WEB", "Phone", "SocialMedia", "Email", "Operations", "BoardofDirectors","Letter","App","WalkIn","Five11"))
      )),
      
      fluidRow(
        column(width = 6, align = "center",
               checkboxGroupInput(inputId = "title_vi", label = "Title VI", inline = TRUE, 
                             selected = c("Y", "N"),
                             choices = c("Y", "N"))),
        column(width = 5, align = "center", 
               checkboxGroupInput(inputId = "ada", label = "ADA Complaints", inline = TRUE, 
                             selected = c("Y", "N"),
                             choices = c("Y", "N")))
      ),
      
      fluidRow(column(width = 12, 
        pickerInput(inputId = "cities", label = "Incident City", width = "100%", 
                    choices = sort(unlist(cus_rel_data %>% select(IncidentCity) %>% unique(), use.names = FALSE)),
                    selected = unlist(cus_rel_data %>% select(IncidentCity) %>% unique(), use.names = FALSE),
                    options = list('actions-box' = TRUE, 'live-search' = TRUE, 'title' = 'Select Cities', 'live-search-placeholder' = 'Search for Cities', 'selected-text-format' = 'count > 3', 'size' = 5),
                    multiple = TRUE),
        pickerInput(inputId = "routes", label = "Route", width = "100%", 
                    choices = cus_rel_data %>% pull(Route) %>% unique(), 
                    selected = cus_rel_data %>% pull(Route) %>% unique(),
                    options = list('actions-box' = TRUE, 'live-search' = TRUE, 'title' = 'Select Routes', 'live-search-placeholder' = 'Search for Routes', 'selected-text-format' = 'count > 3', 'size' = 5),
                    multiple = TRUE),
        pickerInput(inputId = "reasons", label = "Complaint Reason", width = "100%", 
                    choices = sort(cus_rel_data %>% select("Reason1", "Reason2") %>% t %>% c %>% unique),
                    selected = sort(cus_rel_data %>% select("Reason1", "Reason2") %>% t %>% c %>% unique),
                    options = list('actions-box' = TRUE, 'live-search' = TRUE, 'title' = 'Select Reasons', 'live-search-placeholder' = 'Search for Reasons', 'selected-text-format' = 'count > 3', 'size' = 5), 
                    multiple = TRUE),
        pickerInput(inputId = "department", label = "Department", width = "100%", 
                    choices = sort(unique(cus_rel_data$ForAction)), 
                    selected = sort(unique(cus_rel_data$ForAction)),
                    options = list('actions-box' = TRUE, 'live-search' = TRUE, 'title' = 'Select Departments', 'live-search-placeholder' = 'Search for Departments', 'selected-text-format' = 'count > 3', 'size' = 5),
                    multiple = TRUE)
      ))
    ),
  
    # Show a plot of the generated distribution
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),
      fluidRow(
        tabBox(id = "tabset1", width = 12, 
          tabPanel("Main Map", 
                   withSpinner(leafletOutput("point_map", height = "600px"), 
                               type = getOption("spinner.type", 6), 
                               color = getOption("spinner.color", "#00a65a"),
                               hide.ui = FALSE)), 
          tabPanel("Communities of Concern Map", 
                   fluidRow(column(width = 12,
                     absolutePanel(actionButton("refreshCoC", "Refresh Map", icon = icon("sync-alt")), right = "20px", top = "5px", style = "z-index: 1;"),
                     withSpinner(leafletOutput("CoC_map", height = "600px"), 
                                 type = getOption("spinner.type", 6), 
                                 color = getOption("spinner.color", "#00a65a"),
                                 hide.ui = FALSE),
                     h5(tags$a(href="https://opendata.mtc.ca.gov/datasets/28a03a46fe9c4df0a29746d6f8c633c8_0", "More About the Communities of Concern Data Set"))))), 
          tabPanel("Graphs", 
                   fluidRow(
                     column(width = 3, 
                            box(width = 12, 
                                prettyRadioButtons(inputId = "graph_var", label = "Select an Independent Variable: ", 
                                                   choices = c("Bus Route", "Complaint Reason", "Contact Source", "Incident City")))), 
                     column(width = 9, 
                            plotOutput("thePlots"))
                   )), 
          tabPanel("Tables", 
                   fluidRow(
                     column(width = 3, 
                            box(width = 12, 
                                prettyRadioButtons(inputId = "table_var", label = "Select an Independent Variable: ", 
                                                   choiceNames = c("Bus Route", "Complaint Reason", "Contact Source", "Incident City"), 
                                                   choiceValues = c("Route", "Reason1", "ContactSource", "IncidentCity")))), 
                     column(width = 9, 
                            DTOutput("summaryTable"))
                   )), 
          tabPanel("Raw Data", 
                   DTOutput("dataTable"), 
                   downloadButton("downloadCSV", "Download as CSV")
                   )
        )
      )
    )
)

# Define server logic
server <- function(input, output){
  # Leaflet color palettes
  #colors <- c("#00a65a", "#dd4b39")
  #levels <- c("Normal", "High")
  #priority_palette <- colorFactor(colors, levels = levels)
  
  contact_sources <- unique(cus_rel_data$ContactSource) # get unique contact sources
  contact_source_labels <- c("WEB", "Phone", "Social Media", "Email", "Board of Directors", "Letter", "Operations", "App", "Walk In", "Five11")
  contact_source_palette <- colorFactor(palette="Set3", domain=contact_sources)
  color_list <- contact_source_palette(contact_sources)
  
  coc_palette <- colorNumeric(c("white", "#db1a02"), domain = c(0, 1))
  # Heatmap hover labels
  Labels <- str_c("<b>Complaints:</b> ", cus_rel_coc$n) %>% lapply(htmltools::HTML)
  
  # Initialize subset polygon to bbox around entire data
  subset_polygon <- reactiveVal(bbox)
  
  # Filtered rows text
  filteredRowsText <- reactiveVal(paste("Selected", nrow(cus_rel_data), "of", nrow(cus_rel_data), "complaints"))
  
  # Map Output
  output$point_map <- renderLeaflet({
    cus_rel_data %>%
      leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(lat = ~jitter(Latitude, factor = 6), lng = ~jitter(Longitude, factor = 6), 
                       fill = TRUE, fillColor = ~contact_source_palette(ContactSource), 
                       fillOpacity = 0.6, stroke = TRUE, 
                       radius = 8, 
                       color = "#000", weight = 4, opacity = 0.1,
                       popup = ~Label,
                       group = "circlemarkers") %>%
      addDrawToolbar(polylineOptions=F, 
                     circleOptions=F, 
                     markerOptions=F, 
                     circleMarkerOptions=F, 
                     singleFeature=T, 
                     rectangleOptions=drawRectangleOptions(showArea=FALSE, repeatMode=TRUE),
                     editOptions=editToolbarOptions(remove=TRUE)) %>%
      addLegend("bottomright", colors = color_list, labels = contact_source_labels, title = "Contact Source")
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
    dplyr::filter(cus_rel_sf, 
           Priority %in% input$priorities,
           ReceivedDateDay %in% input$receiveddateday,
           IncidentCity %in% input$cities, 
           Route %in% input$routes,
           RespondVia %in% input$respondVia, 
           ForAction %in% input$department, 
           Reason1 %in% input$reasons | Reason2 %in% input$reasons,
           between(ReceivedDate, input$date[1], input$date[2]),
           ContactSource %in% input$contact,
           TitleVI %in% input$title_vi,
           ADAComplaint %in% input$ada,
           st_within(geometry, subset_polygon(), sparse=FALSE))
  )
  # Update Map and filteredRowsText After Options are Changed
  observeEvent(filtered_data(), {
    proxy <- leafletProxy("point_map", data = filtered_data()) %>%
      clearGroup("circlemarkers") %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(lat = ~jitter(Latitude, factor = 6), lng = ~jitter(Longitude, factor = 6), 
                       fill = TRUE, fillColor = ~contact_source_palette(ContactSource), 
                       fillOpacity = 0.6, stroke = TRUE, 
                       radius = 8, 
                       color = "#000", weight = 4, opacity = 0.1, 
                       popup = ~Label,
                       group = "circlemarkers")
      filteredRowsText(paste("Selected", nrow(filtered_data()), "of", nrow(cus_rel_data), "complaints"))
  })
  
  # Update map when user updates rectangle from drawToolba
  observeEvent(input$point_map_draw_all_features, {
    features <- input$point_map_draw_all_features$features
    # No features found (e.g. user deleted shapes)
    if (length(features) == 0){
      subset_polygon(bbox)
    } else {
      feat <- features[[1]] # we only allow one feature at time in drawtoolbar
      coords <- unlist(feat$geometry$coordinates)
      coords <- matrix(coords, ncol = 2, byrow = T)
      poly <- st_sf(st_sfc(st_polygon(list(coords))), crs = 4326)
      subset_polygon(poly)
    }
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
                           "<b>CoC Class:</b> ", coc_class, "<br/>", 
                           "<b>% Low Income:</b> ", round(pct_below2*10, 2), "<br/>", 
                           "<b>% Minority:</b> ", round(pct_minori*10, 2), "<br/>", 
                           "<b>% Disabled:</b> ", round(pct_disab*10, 2), "<br/>", 
                           "<b>% Over 75:</b> ", round(pct_over75*10, 2), "<br/>", 
                           "<b>% 0-Vehicle Household:</b> ", round(pct_zvhhs*10, 2), "<br/>", 
                           "<b>% Single Parent Family:</b> ", round(pct_spfam*10, 2))
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
  
  # Graphs Tab
  output$thePlots <- renderPlot({
    topLevels <- cus_rel_data %>%
      dplyr::count(ContactSource) %>%
      slice_max(order_by=n, n=5) %>%
      arrange(n) %>%
      pull(ContactSource)
      p1 <- filtered_data() %>%
      mutate(Month=month(ReceivedDate),
             PlotVar=if_else(ContactSource %in% topLevels, ContactSource, "Other"),
             PlotVar=fct_relevel(PlotVar, c("Other", topLevels))) %>%
      ggplot() +
      #geom_bar(aes(x=Month, fill=PlotVar)) +
      geom_bar(aes(x=Month, fill=PlotVar), position="fill") +
      scale_x_continuous(breaks=1:12, labels=substr(month.name[1:12], 1, 3)) +
      scale_y_continuous(labels=scales::percent) +
      guides(fill=guide_legend(reverse=TRUE)) +
      labs(fill="ContactSource") +
      # ylab("Frequency")
      ylab("Monthly Proportion")

      p2 <- filtered_data() %>%
      mutate(Month=month(ReceivedDate),
             PlotVar=if_else(ContactSource %in% topLevels, ContactSource, "Other"),
             PlotVar=fct_relevel(PlotVar, c("Other", topLevels))) %>%
      ggplot() +
      #geom_bar(aes(x=Month, fill=PlotVar)) +
      geom_bar(aes(x=Month, fill=PlotVar), position = "stack") +
      scale_x_continuous(breaks=1:12, labels=substr(month.name[1:12], 1, 3)) +
      scale_y_continuous() +
      guides(fill=guide_legend(reverse=TRUE)) +
      labs(fill="ContactSource") +
      # ylab("Frequency")
      ylab("Monthly Proportion")
      grid.arrange(p1,p2, nrow = 2)
  })
  
  # Tables Tab
  output$summaryTable = renderDT(
    datatable(filtered_data() %>% st_drop_geometry() %>% 
                group_by(across(all_of(as.character(input$table_var)))) %>% 
                summarize(Complaints = n()), 
              selection = "none", class = 'row-border stripe nowrap', rownames = FALSE, 
              options = list(scrollX = TRUE, scrollY = "500px", scrollCollapse = TRUE, deferRender = TRUE, 
                             columnDefs = list(list(className = 'dt-left', targets = 0:1))))
  )
  
  # Number of Complaints Shown
  output$filteredRowsText <- renderText({
    filteredRowsText()
  })
  
  # Display filtered data
  output$dataTable = renderDT(
    datatable(filtered_data()[,names(filtered_data()) != "Label"], 
              selection = "none", class = 'row-border stripe nowrap', 
              options = list(scrollX = TRUE, scrollY = "500px", scrollCollapse = TRUE, deferRender = TRUE))
  )
  # Download CSV of filtered complaints
  output$downloadCSV <- downloadHandler(
    filename = "Bus_Complaints.csv",
    content = function(file) {
      write.csv(filtered_data()[,names(filtered_data()) != "Label"], file)
    },
    contentType = "text/csv"
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
