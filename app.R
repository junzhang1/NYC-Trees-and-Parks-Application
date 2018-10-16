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
                              selectInput("BoroughSelect",
                                          "Borough:",
                                          choices = borough[borough!= ""],
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = c("Oak: Bur", "Linden: Silver", "Oak: Northern Red")),
                              radioButtons("boroSelect",
                                           "Borough Filter:",
                                           choices = levels(greenInf.load$borough),
                                           selected = "Bronx")
                            ),




