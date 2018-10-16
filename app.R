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
                              plotlyOutput("plot_condition"),
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
    url <- paste0("https://data.cityofnewyork.us/resource/8ph2-z4iu.json?$select=GISPROPNUM,OMPPROPID,PROPNAME,the_geom,SITENAME,LOCATION,ACRES,SUBCATEGOR,COMMUNITYB,BOROUGH where=RETIRED='FALSE'")
    # print(url)
    
    # Load and clean data
    dat311 <- ckanSQL(url) %>%
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
    url2 <- paste0("https://data.cityofnewyork.us/resource/5rq2-4hqu.json?$select=created_at,tree_id,block_id,the_geom,tree_dbh,health,spc_latin,spc_common,steward,guards,sidewalk,user_type,problems,address,zipcode,zip_city,cb_num,borocode,boroname,st_assem,st_senate,nta_name,state,Latitude,longitude,x_sp,y_sp where=curb_loc='OnCurb' AND status='Alive'")
    dat311b <-ckanSQL(url2) %>%
    return(dat311b)
  }
  )
  
  # Reactive melted data
  meltInput <- reactive({
    load311b() %>%
      melt(id = "tree_id")
  })
  
  # Boxplot showing distribution of diameter at breast height by species
  output$plot_dbh <- renderPlotly({
    dat <- load311b() %>%
      filter(treedbh != 0)
    ggplotly(
      ggplot(dat, aes(x = spc_common, y = treedbh)) + geom_boxplot() + 
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
    
    subset(dat311b, select = c(tree_id,tree_dbh,health,spc_common,address,zipcode,Latitude,longitude,))
  })
  
  # Download data in the datatable
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("city-of-pittsburgh-trees-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(load311(), file)
    }
  )