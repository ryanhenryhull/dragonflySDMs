###############################################################################
# Author: Ryan Hull
# Date: February 2026
# Purpose: Generate interactive shiny app for false negativity of rf predictions
###############################################################################




# Libraries
library(shiny)
library(leaflet)
library(rsconnect)
library(sf)

# read in data, path relative to the directory home of app.R
pfaf_false_negativity_results <- st_read("data/results/pfaf_false_negativity_results.gpkg")


# ui
ui <- fluidPage(
  titlePanel("False Negative Rate"),
  leafletOutput("map")
)

# server
server <- function(input,output,session){
  
  palette <- colorNumeric(
    palette = "magma",
    domain = pfaf_false_negativity_results$false_negativity
  )
  
  output$map <- renderLeaflet({
    leaflet(data = pfaf_false_negativity_results) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%  # light grey background
      addPolygons(
        fillColor = ~palette(false_negativity),
        fillOpacity = 0.8,
        color = "white",
        weight = 1,
        popup = ~paste("Value:", false_negativity)
      ) %>%
      addLegend(
        pal= palette,
        values = ~false_negativity,
        title = "RF SDM False Negativity")
    })
}

# run it
shinyApp(ui=ui, server=server)
