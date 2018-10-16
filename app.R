#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
acres <- sort(ckanUniques("8ph2-z4iu", "ACRES")$ACRES)
health <- sort(ckanUniques("5rq2-4hqu", "health")$health)
treedbh<- sort(ckanUniques("5rq2-4hqu", "tree_dbh")$tree_dbh)


# Define UI for application
ui <- navbarPage("NYC Trees and Parks",
                 theme = shinytheme("united"),
                 tabPanel("Map",
                          sidebarLayout(
                            sidebarPanel(
                              # Borough select
                              selectInput("BoroughSelect",
                                          "Borough:",
                                          choices = borough[borough!= ""],
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = c("Queens", "Brooklyn", "Manhattan")),

                              # Health select
                              checkboxGroupInput("HealthSelect", "Health:",
                                                 choices = health[health!= ""],
                                                 selected = c("Fair","Good")),

                              # Reset select
                              actionButton("reset", "Reset Filters", icon = icon("refresh"))
                            ),
                            mainPanel(
                              # Style the background and change the page
                              tags$style(type = "text/css", ".leaflet {height: calc(100vh - 90px) !important;}
                                         body {background-color: #D4EFDF;}"),
                              # Map Output
                              leafletOutput("leaflet")
                              )
                            )
                          ),
                 # Graphs
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              # Acres select
                              sliderInput("AcresSelect",
                                          "Acres:",
                                          min = min(as.numeric(acres), na.rm=T),
                                          max = max(as.numeric(acres), na.rm=T),
                                          value = c(min(as.numeric(acres), na.rm=T), max(as.numeric(acres), na.rm=T)),
                                          step = 1),
                              
                              # Tree Diameter at Breast Height Select
                              sliderInput("DBHSelect",
                                          "Tree DBH:",
                                          min = min(as.numeric(treedbh), na.rm=T),
                                          max = max(as.numeric(treedbh), na.rm=T),
                                          value = c(min(as.numeric(treedbh), na.rm=T), max(as.numeric(treedbh), na.rm=T)),
                                          step = 1),

                              # Reset select
                              actionButton("reset", "Reset Filters", icon = icon("refresh"))
                            ),
                            mainPanel(
                              plotlyOutput("plot_dbh"),
                              br(),
                              br(),
                              plotlyOutput("plot_species"),
                              br()
                            )
                          )
                          
                 ),
                 # Data Table
                 tabPanel("Table",
                          inputPanel(
                            downloadButton("downloadData","Download NYC Trees and Parks Data")
                          ),
                          fluidPage(DT::dataTableOutput("table"))
                 )
)
                            
# Define server logic
server <- function(input, output,session = session) {
  load311 <- reactive({
    
    # Build API Query with proper encodes
    commonNameFilter <- ifelse(length(input$CommonNameSelect) > 0, paste0("%20AND%20%22common_name%22%20IN%20(%27", paste0(gsub(" " ,"%20", input$CommonNameSelect), collapse = "%27,%27"), "%27)"), "")
    conditionFilter <- ifelse(length(input$ConditionSelect) > 0, paste0("%20AND%20%22condition%22%20IN%20(%27", paste0(gsub(" " ,"%20", input$ConditionSelect), collapse = "%27,%27"),"%27)"), "")
    # url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%221515a93c-73e3-4425-9b35-1cd11b2196da%22%20WHERE%20%22diameter_base_height%22%20%3E=%20%27", input$DiameterBaseHeightSelect[1], "%27%20AND%20%22diameter_base_height%22%20%3C=%20%27", input$DiameterBaseHeightSelect[2], "%27%20AND%20%22common_name%22%20IN%20(%27", commonNameFilter, "%27)%20AND%20%22condition%22%20IN%20(%27", conditionFilter, "%27)")
    # Your code
  
    
    
    url <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%221515a93c-73e3-4425-9b35-1cd11b2196da%22%20WHERE%20%22diameter_base_height%22%20%3E=%20%27", input$DiameterBaseHeightSelect[1], "%27%20AND%20%22diameter_base_height%22%20%3C=%20%27", input$DiameterBaseHeightSelect[2], "%27", commonNameFilter, conditionFilter)
    
    print(url)
    
    # Load and clean data
    dat311 <- ckanSQL(url1) %>%
      mutate(
        # Change borough to full text
        BOROUGH = case_when(
          BOROUGH == "Q" ~ "Queens",
          BOROUGH == "B" ~ "Brooklyn",
          BOROUGH == "M" ~ "Manhattan",
          BOROUGH == "X" ~ "Bronx",
          BOROUGH == "R" ~ "Staten Island",
          TRUE ~ as.character(BOROUGH)
      )
      return(dat311)
      )}
    
    load311b <- reactive({
      
      
      
    })
      
    
    # Reactive melted data
    meltInput <- reactive({
      load311() %>%
        melt(id = "id")
    }) 
}
