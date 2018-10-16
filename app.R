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
  data.frame(jsonlite::fromJSON(json)$result$records)
}

# Unique values for Resource Field
# Data Source: https://data.cityofnewyork.us/City-Government/Parks-Zones/rjaj-zgq7
ckanUniques <- function(field) {
  url <- paste0("https://data.cityofnewyork.us/resource/uyfj-5xid.json?$select=", field, "&$group=", field)
  c(ckanSQL(URLencode(url)))
}
# Data Source: https://data.cityofnewyork.us/Environment/2015-Street-Tree-Census-Tree-Data/pi5s-9p35
ckanUniques <- function(field) {
  url2 <- paste0("https://data.cityofnewyork.us/resource/5rq2-4hqu.json?$select=", field, "&$group=", field)
  c(ckanSQL(URLencode(url2)))
}



# Define UI for application
ui <- navbarPage("NYC Subway Entrances",
                 theme = shinytheme("united"),
                 tabPanel("Map",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("sewerSelect",
                                          "Sewer Type",
                                          levels(greenInf.load$sewer_type),
                                          selected = c("MS4", "Non-combined"),
                                          selectize = T,
                                          multiple = T),
                              radioButtons("boroSelect",
                                           "Borough Filter:",
                                           choices = levels(greenInf.load$borough),
                                           selected = "Bronx")
                            ),




