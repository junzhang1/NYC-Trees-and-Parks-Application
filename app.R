# Jun Zhang's project 2
# The application is about NYC trees and parks. I used two data, trees data and parks data, to build the app.
# There're three pages, a map where users can find parks area and trees in NYC, and they can customerize the map by selecting
# boroughs and tree diameter at breast height (d.b.h.). The second page consists of two plots showing distribution of tree
# diameter at breast height by species and health condition by species. Users can customerize the plots by selecting health
# condition and council district. The third page is a table. The data are extracted from trees data, and users can download them.

library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(reshape2)
library(plyr)
library(dplyr)
library(plotly)
library(stringr)
library(ggplot2)
library(scales)
library(httr)
library(jsonlite)
library(htmltools)
library(sf)

ckanSQL <- function(url) {
  # Make the Request
  r <- RETRY("GET", URLencode(url))
  # Extract Content
  c <- content(r, "text")
  # Basic gsub to make NA's consistent with R
  json <- gsub('NaN', 'NA', c, perl = TRUE)
  # Create Dataframe
  # print(json)
  data.frame(jsonlite::fromJSON(json))
}

# Unique values for Resource Field
# Data Source: https://data.cityofnewyork.us/City-Government/Parks-Zones/rjaj-zgq7
# Data Source: https://data.cityofnewyork.us/Environment/2015-Street-Tree-Census-Tree-Data/pi5s-9p35
ckanUniques <- function(resource, field) {
  url <- paste0("https://data.cityofnewyork.us/resource/", resource, ".json?$select=", field, "&$group=", field)
  # print(url)
  c(ckanSQL(URLencode(url)))
}


# Grab the unique values for the 4 inputs
borough <- sort(ckanUniques("8ph2-z4iu", "BOROUGH")$BOROUGH)
cncldist <- sort(ckanUniques("5rq2-4hqu", "cncldist")$cncldist)
health <- sort(ckanUniques("5rq2-4hqu", "health")$health)
treedbh<- sort(ckanUniques("5rq2-4hqu", "tree_dbh")$tree_dbh)

borough <- borough %>%
  as_tibble() %>%
  mutate(
    # Change borough to full text
    BOROUGH = case_when(
      value == "Q" ~ "Queens",
      value == "B" ~ "Brooklyn",
      value == "M" ~ "Manhattan",
      value == "X" ~ "Bronx",
      value == "R" ~ "Staten Island",
      TRUE ~ as.character(value)
    ))

# Define UI for application
ui <- navbarPage("NYC Trees and Parks Zones",
                 theme = shinytheme("united"),
                 tabPanel("Map",
                          sidebarLayout(
                            sidebarPanel(
                              # Borough select
                              selectInput("BoroughSelect",
                                          "Borough for Parks:",
                                          choices = borough$BOROUGH,
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = c("Queens", "Brooklyn", "Manhattan")),
                              # Tree Diameter at Breast Height Select
                              sliderInput("DBHSelect",
                                          "Tree Diameter at Breast Height:",
                                          min = min(as.numeric(treedbh), na.rm=T),
                                          max = max(as.numeric(treedbh), na.rm=T),
                                          value = c(min(as.numeric(treedbh), na.rm=T), max(as.numeric(treedbh), na.rm=T)),
                                          step = 1),

                              # Reset select
                              actionButton("reset1", "Reset Filters", icon = icon("refresh"))
                            ),
                            mainPanel(
                              # Style the background and change the page
                              tags$style(type = "text/css", ".leaflet {height: calc(100vh - 90px) !important;}
                                         body {background-color: #c0d8f7;}"),
                              # Map Output
                              leafletOutput("leaflet")
                              )
                            )
                          ),
                 # Graphs
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(

                              # Health select
                              checkboxGroupInput("HealthSelect", "Health Condition:",
                                                 choices = health,
                                                 selected = c("Fair","Good")),
                              # Council District select
                              sliderInput("CncldistSelect",
                                          "Council District:",
                                          min = min(as.numeric(cncldist), na.rm=T),
                                          max = max(as.numeric(cncldist), na.rm=T),
                                          value = c(min(as.numeric(cncldist), na.rm=T), max(as.numeric(cncldist), na.rm=T)),
                                          step = 1),

                              # Reset select
                              actionButton("reset2", "Reset Filters", icon = icon("refresh"))
                            ),
                            mainPanel(
                              plotlyOutput("plot_dbh"),
                              br(),
                              br(),
                              plotlyOutput("plot_condition"),
                              br()
                            )
                          )
                          
                 ),
                 # Data Table
                 tabPanel("Table",
                          inputPanel(
                            downloadButton("downloadData","Download NYC Trees and Parks Zones Data")
                          ),
                          fluidPage(DT::dataTableOutput("table"))
                 )
)
                            
# Define server logic
server <- function(input, output,session = session) {
  load311 <- reactive({
    
    # Build API Query with proper encodes
    if (length(input$BoroughSelect) > 0) {
      borough_abrv <- input$BoroughSelect %>%
      as_tibble() %>%
      mutate(
        abrv_borough = case_when(
          value == 'Queens' ~ 'Q',
          value == 'Brooklyn' ~ 'B',
          value == 'Manhattan' ~ 'M',
          value == 'Bronx' ~ 'X',
          value == 'Staten Island' ~ 'R'
        )
      )
      
      boroughFilter <- paste0("%20AND%20BOROUGH%20IN%20(%27", paste0(borough_abrv$abrv_borough, collapse = "','"), "')")
    } else {
      boroughFilter <- ""
    }
    
    
    url <- paste0("https://data.cityofnewyork.us/resource/8ph2-z4iu.geojson?$query=SELECT%20GISPROPNUM,the_geom,PROPNAME,SITENAME,LOCATION,ACRES,SUBCATEGOR,BOROUGH%20WHERE%20RETIRED=%27False%27", boroughFilter)
    url <- gsub(" ", "%20", url)
    print(url)
    
    # Load and clean data
    dat311 <- readOGR(url) %>%
      sf::st_as_sf(.) %>%
      mutate(
        # Change borough to full text
        BOROUGH = case_when(
          BOROUGH == "Q" ~ "Queens",
          BOROUGH == "B" ~ "Brooklyn",
          BOROUGH == "M" ~ "Manhattan",
          BOROUGH == "X" ~ "Bronx",
          BOROUGH == "R" ~ "Staten Island",
          TRUE ~ as.character(BOROUGH)
      ))

    return(dat311)
  }
  )
  
  load311b <- reactive({
    # Build API Query with proper encodes
    healthFilter <- ifelse(length(input$HealthSelect) > 0, paste0("%20AND%20health%20IN%20(%27", paste0(gsub(" " ,"%20", input$HealthSelect), collapse = "%27,%27"), "%27)"), "")
    url2 <- paste0("https://data.cityofnewyork.us/resource/5rq2-4hqu.json?$query=SELECT%20created_at,tree_id,block_id,boroname,tree_dbh,health,spc_latin,spc_common,problems,address,zipcode,zip_city,state,cncldist,Latitude,longitude,x_sp,y_sp%20WHERE%20curb_loc=%27OnCurb%27AND%20status=%27Alive%27AND%20tree_dbh >= '", input$DBHSelect[1], "' AND tree_dbh <= '", input$DBHSelect[2], "'AND%20cncldist >= '", input$CncldistSelect[1], "' AND cncldist <= '", input$CncldistSelect[2], "'", healthFilter)
    print(url2)
    
    url2 <- gsub(" ", "%20", url2)
    
    # Load and mutate data
    dat311b <-ckanSQL(url2) %>%
      mutate(
             tree_dbh = as.numeric(tree_dbh),
             tree_id = as.factor(tree_id),
             spc_common = as.character(spc_common),
             zipcode = as.numeric(zipcode),
             longitude = as.numeric(longitude),
             Latitude = as.numeric(Latitude)
      )
    
    return(dat311b)
  })
  
  # Reactive melted data
  meltInput <- reactive({
    load311b() %>%
      melt(id = "tree_id")
  })
  
  # Boxplot showing distribution of diameter at breast height by species
  output$plot_dbh <- renderPlotly({
    dat <- load311b() %>%
      filter(tree_dbh != 0)
    ggplotly(
      ggplot(dat, aes(x = spc_common, y = tree_dbh)) + geom_boxplot() + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=0.5)) + 
        ggtitle("Distribution of Diameter at Breast Height by Species") + xlab("Common Name") + ylab("Diameter at Breast Height")
    )
  }) 
  
  # Bar chart showing count, common name and condition
  output$plot_condition <- renderPlotly({
    dat <- load311b()
    ggplotly(
      ggplot(data = dat, aes(x = spc_common, fill = health)) +
        geom_bar() + ggtitle("Count by Species") + xlab("Common Name") + ylab("Count") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=0.5)) +
        guides(color = FALSE))
  })
  
  # Data Table
  output$table <- DT::renderDataTable({
    dat311b <- load311b()
    
    subset(dat311b, select = c(tree_id,tree_dbh,health,spc_common,address,zipcode,Latitude,longitude))
  }, filter = 'top', options = list(scrollX = TRUE, pageLength = 10))
  
  # Map
  output$leaflet <- renderLeaflet({
    # Load green infrastructure filtered data
    map1 <- load311()
    map2 <- load311b()

    # Build Map
    pal <- colorFactor(c("#ed3131", "#8bf738", "#42f7c7", "#6e67fc", "#f762a0"),c("Brooklyn", "Manhattan", "Queens", "Staten Island", "Bronx"))
    leaflet() %>%
      addProviderTiles(providers$Wikimedia, group = "Base", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "TonerLite", options = providerTileOptions(noWrap = TRUE)) %>%
      addLayersControl(
        baseGroups = c("Base", "TonerLite"),
        options = layersControlOptions(collapsed = FALSE)) %>%
    
      # Add points "trees".
      addMarkers(data = map2, popup = ~htmlEscape(spc_common)) %>%
      # Add polygons "parks".
      addPolygons(data = map1,smoothFactor = 0.2, fillOpacity = 1,color=~pal(BOROUGH)) %>%
      # Add legend.
      addLegend(position = "bottomright" , pal = pal, values = map1$BOROUGH, title = "Borough", 
                opacity = 1)
  })
  
  
  # Download data in the datatable
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("NYC-trees-parks", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(load311b(), file)
    }
  )
  
  # Reset Filter Data
  observeEvent(input$reset1, {
    updateSelectInput(session, "BoroughSelect", selected = c("Queens", "Brooklyn", "Manhattan"))
    updateSliderInput(session, "DBHSelect", value = c(min(as.numeric(treedbh), na.rm=T), max(as.numeric(treedbh), na.rm=T)))
    showNotification("You have successfully reset the filters", type = "message")
  })
  # Reset Filter Data
  observeEvent(input$reset2, {
    updateSliderInput(session, "CncldistSelect", value = c(min(as.numeric(cncldist), na.rm=T), max(as.numeric(cncldist), na.rm=T)))
    updateCheckboxGroupInput(session, "HealthSelect", selected = c("Fair","Good"))
    showNotification("You have successfully reset the filters", type = "message")
  })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")